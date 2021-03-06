{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Handler.Contact
  ( getContactR,
    postContactR,
  )
where

import Control.Lens
import Import
import Network.AWS
import Network.AWS.SES.SendEmail
import Network.AWS.SES.Types
import Yesod.ReCaptcha2

data EmailData = EmailData
  { emailFrom :: Maybe Text,
    emailSubject :: Text,
    emailMessage :: Textarea
  }

emailForm :: Form EmailData
emailForm =
  renderDivs
    ( EmailData
        <$> aopt emailField "Your email address" Nothing
        <*> areq textField "Subject" Nothing
        <*> areq textareaField "Message" Nothing
        <* reCaptchaInvisible
    )

getContactR :: Handler Html
getContactR = do
  (widget, enctype) <- generateFormPost emailForm
  messages <- getMessages
  renderContactForm widget enctype messages

renderContactForm :: Widget -> Enctype -> [(Text, Html)] -> Handler Html
renderContactForm widget enctype messages = do
  (reCaptchaFormId, reCaptchaWidget, reCaptchaButtonAttributes) <-
    reCaptchaInvisibleForm Nothing Nothing

  defaultLayout $ do
    setTitle "Callum's Code - contact"
    $(widgetFile "contact")

postContactR :: Handler Html
postContactR = do
  master <- getYesod
  let settings = appSettings master
  messages <- getMessages
  ((result, widget), enctype) <- runFormPost emailForm
  case result of
    FormSuccess r -> do
      (void . runResourceT . runAWS master . send) $
        sendEmail
          (noreplyEmail settings)
          (destination & dToAddresses .~ [adminEmail settings])
          ( message
              (content ("[CC/CONTACT] " <> emailSubject r))
              (body & bText ?~ content (unTextarea (emailMessage r)))
          )
          & maybe id ((seReplyToAddresses .~) . pure) (emailFrom r)

      addMessage "message-success" "Your message has been sent"
      redirect ContactR
    _ -> renderContactForm widget enctype messages
