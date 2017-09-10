{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Handler.Blog where

import Import hiding (toLower)

import Common
import Database.Persist.Sql
import Data.Time.Format
import Widget.Editor

data SimpleBlog = SimpleBlog {
  simpleBlogTitle :: Text,
  simpleBlogContent :: Html
}

blogForm :: FormInput Handler SimpleBlog
blogForm = SimpleBlog <$>
  ireq textField "title" <*>
  ireq htmlField "content"

nbsp :: Char -> Char
nbsp ' ' = '\xA0' 
nbsp x = x

machineTime :: UTCTime -> Html
machineTime = toHtml.fmap nbsp.formatTime defaultTimeLocale
  (iso8601DateFormat.Just$ "%H:%M:%S")

humanTime :: UTCTime -> Html
humanTime = toHtml.fmap nbsp.formatTime defaultTimeLocale "%d %b %Y"

getMorePosts :: Bool -> Handler [BlogPost]
getMorePosts auth = (fmap.fmap) entityVal.runDB$
  selectList (liveFilter auth) [Desc BlogPostTimeCreated]
    where liveFilter True = []
          liveFilter False =  [BlogPostDeleted ==. False]

getBlogOldR :: Int64 -> Handler Html
getBlogOldR blogId = do
  post <- runDB.get404.toSqlKey$ blogId
  redirectWith status301$
    BlogR (blogPostYear post) (blogPostMonth post) (blogPostUrl post)

getBlogR :: Year -> Month -> Text -> Handler Html
getBlogR year month name = do
  auth <- (== Authorized) <$> isAdmin
  Entity _ blogPost <- runDB.getBy404$ UniqueBlogPost year month name
  morePosts <- getMorePosts auth
  editMode <- any (== "edit").fmap fst.reqGetParams <$> getRequest
  
  defaultLayout$ do
    addScript$ StaticR js_jquery_3_2_1_min_js
    addScript$ StaticR js_liveness_js

    let isNewPage = False
    let mPermalink = Just$ BlogR year month name
    let title = blogPostTitle blogPost
    let content = blogPostContent blogPost
    let mtime = Just$
                (blogPostTimeCreated blogPost,
                 blogPostTimeUpdated blogPost)
    setTitle.(++ "  - Callum's Code").toHtml.blogPostTitle$ blogPost 
    when (not editMode)$ do
      addScript$ StaticR js_prism_js
      toWidgetHead$ [julius| window.MathJax = { showMathMenu: false }; |]
      addScriptRemote$ "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_CHTML"
      addStylesheet$ StaticR css_prism_css
    $(widgetFile "blog")

postBlogR :: Year -> Month -> Text -> Handler ()
postBlogR year month name = do
  simple <- runInputPost blogForm
  Entity blogId _ <- runDB.getBy404$ UniqueBlogPost year month name

  now <- liftIO$ getCurrentTime
  runDB$ update blogId [
    BlogPostTitle =. simpleBlogTitle simple,
    BlogPostContent =. simpleBlogContent simple,
    BlogPostTimeUpdated =. now]

  return ()

postBlogLivenessR :: Year -> Month -> Text -> Handler Value
postBlogLivenessR year month name = do
  live <- runInputPost$ ireq checkBoxField "live"
  Entity blogId _ <- runDB.getBy404$ UniqueBlogPost year month name
  runDB$ update blogId [BlogPostDeleted =. (not live)]
  return$ object ["live" .= live]

getBlogNewR :: Handler Html
getBlogNewR = do
  auth <- (== Authorized) <$> isAdmin
  morePosts <- getMorePosts auth

  defaultLayout $ do
    addScript$ StaticR js_jquery_3_2_1_min_js
    addScript$ StaticR js_liveness_js

    let editMode = True
    let isNewPage = True
    let mPermalink = Nothing
    let title = "Title here" :: Text
    let content = preEscapedToMarkup ("<p>Your post here</p>" :: Text)
    let mtime = Nothing
    setTitle "New blog post"
    $(widgetFile "blog")

postBlogNewR :: Handler Html
postBlogNewR = do
  now <- liftIO getCurrentTime
  let (y, m, _) = toGregorian$ utctDay now
  case (,) <$> (mkYear$ fromIntegral y) <*> (mkMonth$ m - 1) of
    Just (year, month) -> do
      simple <- runInputPost blogForm
      let url = mkURL$ simpleBlogTitle simple
      _ <- runDB.insert$ BlogPost {
        blogPostDeleted = True,
        blogPostTitle = simpleBlogTitle simple,
        blogPostContent = simpleBlogContent simple,
        blogPostYear = year,
        blogPostMonth =  month,
        blogPostUrl =  url,
        blogPostTimeCreated = now,
        blogPostTimeUpdated = now
      }
      redirect (BlogR year month url, [("edit" :: Text, "")])
    Nothing -> error "Today is an invalid date!!!"

