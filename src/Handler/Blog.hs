{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
module Handler.Blog where

import           Import

import           Common
import           Database.Persist.Sql
import           Data.Time.Format
import           Text.Blaze.Html.Renderer.Text
import           Text.HTML.TagSoup
import           Widget.Editor
import           Yesod.AtomFeed

data SimpleBlog = SimpleBlog {
  simpleBlogTitle :: Text,
  simpleBlogContent :: Html
}

blogForm :: FormInput Handler SimpleBlog
blogForm = SimpleBlog <$> ireq textField "title" <*> ireq htmlField "content"

nbsp :: Char -> Char
nbsp ' ' = '\xA0'
nbsp x   = x

machineTime :: UTCTime -> Html
machineTime = toHtml . fmap nbsp . formatTime
  defaultTimeLocale
  (iso8601DateFormat . Just $ "%H:%M:%S")

humanTime :: UTCTime -> Html
humanTime = toHtml . fmap nbsp . formatTime defaultTimeLocale "%d %b %Y"

getAllPosts :: Bool -> Handler [BlogPost]
getAllPosts auth = (fmap . fmap) entityVal . runDB $ selectList
  (liveFilter auth)
  [Desc BlogPostTimeCreated]
 where
  liveFilter True  = []
  liveFilter False = [BlogPostDeleted ==. False]

feedEntry :: BlogPost -> FeedEntry (Route App)
feedEntry post = FeedEntry
  { feedEntryLink      = BlogR (blogPostYear post)
                               (blogPostMonth post)
                               (blogPostUrl post)
  , feedEntryUpdated   = blogPostTimeUpdated post
  , feedEntryTitle     = blogPostTitle post
  , feedEntryContent   = blogPostContent post
  , feedEntryEnclosure = Nothing
  }

getFeedR :: Handler RepAtom
getFeedR = do
  posts <- getAllPosts False
  let arbitraryDate = UTCTime (fromGregorian 1970 1 1) 0
  let latest =
        maybe arbitraryDate blogPostTimeUpdated
          . fmap (maximumBy (comparing blogPostTimeCreated))
          . fromNullable
          $ posts
  let entries = fmap feedEntry posts

  atomFeed Feed
    { feedTitle       = "Callum's Code Blog"
    , feedLinkSelf    = FeedR
    , feedLinkHome    = HomeR
    , feedAuthor      = "Callum Lowcay"
    , feedDescription = toHtml
      ("<p>Typical programming blog, mostly short and technical</p>" :: Text)
    , feedLanguage    = "en-nz"
    , feedUpdated     = latest
    , feedLogo        = Nothing
    , feedEntries     = entries
    }

formatPreBlocks :: Html -> Html
formatPreBlocks =
  preEscapedToMarkup
    . renderTags
    . mconcat
    . fmap removeBreaks
    . findBlocks (~== ("<pre>" :: String)) (~== ("</pre>" :: String))
    . parseTags
    . toStrict
    . renderHtml
 where
  removeBreaks [] = []
  removeBreaks (t : tags) =
    if t ~== ("<pre>" :: String) then t : fmap removeBreaks' tags else t : tags
  removeBreaks' :: Tag Text -> Tag Text
  removeBreaks' tag =
    if tag ~== ("<br>" :: String) then TagText "\r\n" else tag
  findBlocks open close ls = case break open ls of
    (r, blk') -> case break close blk' of
      (blk, []    ) -> r : [blk]
      (blk, c : rs) -> r : (blk <> [c]) : findBlocks open close rs

stripTags :: Text -> Text
stripTags = renderTags . filter isTagText . parseTags

getBlogOldR :: Int64 -> Handler Html
getBlogOldR blogId = do
  post <- runDB . get404 . toSqlKey $ blogId
  redirectWith status301
    $ BlogR (blogPostYear post) (blogPostMonth post) (blogPostUrl post)

getBlogR :: Year -> Month -> Text -> Handler Html
getBlogR year month name = do
  auth              <- (== Authorized) <$> isAdmin
  Entity _ blogPost <- runDB . getBy404 $ UniqueBlogPost year month name
  morePosts         <- getAllPosts auth
  editMode          <- elem "edit" . fmap fst . reqGetParams <$> getRequest

  defaultLayout $ do
    atomLink FeedR "RSS"
    addScript $ StaticR js_jquery_3_2_1_min_js

    when auth $ addScript $ StaticR js_liveness_js

    let isNewPage  = False
    let mPermalink = Just $ BlogR year month name
    let title      = blogPostTitle blogPost
    let content    = blogPostContent blogPost
    let mtime =
          Just (blogPostTimeCreated blogPost, blogPostTimeUpdated blogPost)
    setTitle . (++ "  - Callum's Code") . toHtml . blogPostTitle $ blogPost
    unless editMode $ do
      addScript $ StaticR js_prism_js
      toWidgetHead $ [julius| window.MathJax = { showMathMenu: false }; |]
      addScriptRemote
        "//cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-AMS_CHTML"
      addStylesheet $ StaticR css_prism_css
    $(widgetFile "blog")

postBlogR :: Year -> Month -> Text -> Handler ()
postBlogR year month name = do
  simple          <- runInputPost blogForm
  Entity blogId _ <- runDB . getBy404 $ UniqueBlogPost year month name

  now             <- liftIO getCurrentTime
  runDB $ update
    blogId
    [ BlogPostTitle =. stripTags (simpleBlogTitle simple)
    , BlogPostContent =. formatPreBlocks (simpleBlogContent simple)
    , BlogPostTimeUpdated =. now
    ]

  return ()

postBlogLivenessR :: Year -> Month -> Text -> Handler Value
postBlogLivenessR year month name = do
  live               <- runInputPost $ ireq checkBoxField "live"
  Entity blogId blog <- runDB . getBy404 $ UniqueBlogPost year month name

  now                <- liftIO getCurrentTime
  let publish = if blogPostUnpublished blog && live
        then
          [ BlogPostUnpublished =. False
          , BlogPostTimeCreated =. now
          , BlogPostTimeUpdated =. now
          ]
        else []

  runDB $ update blogId ((BlogPostDeleted =. not live) : publish)
  return $ object ["live" .= live]

getBlogNewR :: Handler Html
getBlogNewR = do
  auth      <- (== Authorized) <$> isAdmin
  morePosts <- getAllPosts auth

  defaultLayout $ do
    addScript $ StaticR js_jquery_3_2_1_min_js
    addScript $ StaticR js_liveness_js

    let editMode   = True
    let isNewPage  = True
    let mPermalink = Nothing
    let title      = "Title here" :: Text
    let content = preEscapedToMarkup ("<p>Your post here</p>" :: Text)
    let mtime      = Nothing
    setTitle "New blog post"
    $(widgetFile "blog")

postBlogNewR :: Handler Html
postBlogNewR = do
  now <- liftIO getCurrentTime
  let (y, m, _) = toGregorian $ utctDay now
  case (,) <$> mkYear (fromIntegral y) <*> mkMonth (m - 1) of
    Just (year, month) -> do
      simple <- runInputPost blogForm
      let url = mkURL . stripTags . simpleBlogTitle $ simple
      _ <- runDB . insert $ BlogPost
        { blogPostUnpublished = True
        , blogPostDeleted     = True
        , blogPostTitle       = stripTags $ simpleBlogTitle simple
        , blogPostContent     = formatPreBlocks $ simpleBlogContent simple
        , blogPostYear        = year
        , blogPostMonth       = month
        , blogPostUrl         = url
        , blogPostTimeCreated = now
        , blogPostTimeUpdated = now
        }
      redirect (BlogR year month url, [("edit" :: Text, "")])
    Nothing -> error "Today is an invalid date!!!"
