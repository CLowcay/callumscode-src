{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Handler.File where

import           Import
import           System.Directory

postUploadR :: UploadTarget -> Handler Value
postUploadR target = do
  app  <- getYesod
  info <- runInputPost $ ireq fileField "file"
  let filePath = unpack $ fileName info
  let name = case target of
        UploadBin        -> binFile app filePath
        UploadSrc        -> srcFile app filePath
        UploadScreenshot -> screenshotFile app filePath

  exists <- liftIO $ doesFileExist name
  if exists
    then return $ object
      [ "files"
          .= [ object
                 [ "name" .= fileName info
                 , "error" .= ("File already exists" :: Text)
                 ]
             ]
      ]
    else do
      size <- liftIO $ do
        fileMove info name
        getFileSize name

      render <- getUrlRender

      return $ object
        [ "files"
            .= [ object
                   [ "name" .= fileName info
                   , "size" .= size
                   , "url" .= render (UploadFileR target (fileName info))
                   , "deleteUrl" .= render (UploadFileR target (fileName info))
                   , "deleteType" .= ("DELETE" :: Text)
                   ]
               ]
        ]

getUploadFileR :: UploadTarget -> Text -> Handler Html
getUploadFileR target filename = do
  app <- getYesod
  case target of
    UploadBin -> sendFile typeOctet (binFile app $ unpack filename)
    UploadSrc -> sendFile typeOctet (srcFile app $ unpack filename)
    UploadScreenshot ->
      sendFile typeOctet (screenshotFile app $ unpack filename)

deleteUploadFileR :: UploadTarget -> Text -> Handler Value
deleteUploadFileR target filename = do
  app <- getYesod
  let filePath = case target of
        UploadBin        -> binFile app (unpack filename)
        UploadSrc        -> srcFile app (unpack filename)
        UploadScreenshot -> screenshotFile app (unpack filename)
  liftIO $ removeFile filePath
  return $ object ["files" .= [object [filename .= True]]]
