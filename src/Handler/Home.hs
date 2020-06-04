{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home where

import           Import
import           Widget.Editor

plainPageForm :: FormInput Handler Html
plainPageForm = ireq htmlField "content"

getHomeR :: Handler Html
getHomeR = do
  auth <- (== Authorized) <$> isAdmin
  let editMode   = False
  let mPermalink = Nothing

  mblog <- runDB $ selectFirst [BlogPostDeleted ==. False]
                               [Desc BlogPostTimeCreated, LimitTo 1]
  case mblog of
    Just (Entity _ blog) -> redirect
      $ BlogR (blogPostYear blog) (blogPostMonth blog) (blogPostUrl blog)
    Nothing -> defaultLayout $ do
      setTitle "Callum's Code"
      let content = preEscapedToMarkup
            ("<p class=\"content\">No content here ...</p>" :: Text)
      $(widgetFile "plain-page")

getBlogHomeR :: Handler Html
getBlogHomeR = getHomeR

getAboutR :: Handler Html
getAboutR = getPlainPageR "about" AboutR

postAboutR :: Handler ()
postAboutR = postPlainPageR "about"

getPrivacyR :: Handler Html
getPrivacyR = getPlainPageR "privacy" PrivacyR

postPrivacyR :: Handler ()
postPrivacyR = postPlainPageR "privacy"

getPlainPageR :: Text -> Route App -> Handler Html
getPlainPageR page permalink = do
  auth               <- (== Authorized) <$> isAdmin
  Entity _ plainPage <- runDB $ getBy404 $ UniquePlainPage page

  let mPermalink = Just permalink
  editMode <- elem "edit" . fmap fst . reqGetParams <$> getRequest

  defaultLayout $ do
    setTitle $ "Callum's Code - " ++ toHtml page
    let content = plainPageContent plainPage
    $(widgetFile "plain-page")

postPlainPageR :: Text -> Handler ()
postPlainPageR page = do
  content         <- runInputPost plainPageForm
  Entity pageId _ <- runDB . getBy404 $ UniquePlainPage page

  now             <- liftIO getCurrentTime
  runDB
    $ update pageId [PlainPageContent =. content, PlainPageTimeUpdated =. now]

  pure ()

