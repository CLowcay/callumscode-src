{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Contact
  ( getContactR
  , postContactR
  )
where

import           Control.Lens
import           Import
import           Network.AWS
import           Network.AWS.SES.SendEmail
import           Network.AWS.SES.Types

data EmailData = EmailData {
  emailFrom :: Maybe Text,
  emailSubject :: Text,
  emailMessage :: Textarea
}

emailForm :: Form EmailData
emailForm =
  renderDivs
    $   EmailData
    <$> aopt emailField "Your email address" Nothing
    <*> areq textField     "Subject" Nothing
    <*> areq textareaField "Message" Nothing

getContactR :: Handler Html
getContactR = do
  (widget, enctype) <- generateFormPost emailForm
  messages          <- getMessages

  defaultLayout $ do
    setTitle "Callum's Code - contact"
    $(widgetFile "contact")

postContactR :: Handler Html
postContactR = do
  master <- getYesod
  let settings = appSettings master
  messages                    <- getMessages
  ((result, widget), enctype) <- runFormPost emailForm
  case result of
    FormSuccess r -> do
      (void . runResourceT . runAWS master . send)
        $ sendEmail
            (noreplyEmail settings)
            (destination & dToAddresses .~ [adminEmail settings])
            (message
              (content ("[CC/CONTACT] " <> emailSubject r))
              (body & bText .~ (Just . content . unTextarea $ emailMessage r))
            )
        & (maybe id ((seReplyToAddresses .~) . pure) (emailFrom r))

      addMessage "message-success" "Your message has been sent"
      redirect ContactR

    _ -> defaultLayout $ do
      setTitle "Callum's Code - contact"
      $(widgetFile "contact")
