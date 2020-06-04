{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Contact where

import           Import
import           Network.Mail.Mime
import           Network.Mail.Mime.SES

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
  master                      <- getYesod
  messages                    <- getMessages
  ((result, widget), enctype) <- runFormPost emailForm
  case result of
    FormSuccess r -> do
      let header = maybe "" (\e -> "REPLY TO: " <> e <> "\n\n") $ emailFrom r
          mail   = simpleMail'
            (Address Nothing (noreplyEmail $ appSettings master))
            (Address Nothing (adminEmail $ appSettings master))
            ("[CC/CONTACT] " <> emailSubject r)
            (fromStrict $ header <> unTextarea (emailMessage r))
          ses = SES
            { sesFrom         = encodeUtf8 . noreplyEmail $ appSettings master
            , sesTo           = [encodeUtf8 . adminEmail $ appSettings master]
            , sesAccessKey    = encodeUtf8 . awsKey $ appSettings master
            , sesSecretKey    = encodeUtf8 . awsSecret $ appSettings master
            , sesSessionToken = Nothing
            , sesRegion       = awsSESRegion $ appSettings master
            }

      renderSendMailSES (appHttpManager master) ses mail
      addMessage "message-success" "Your message has been sent"
      redirect ContactR

    _ -> defaultLayout $ do
      setTitle "Callum's Code - contact"
      $(widgetFile "contact")

