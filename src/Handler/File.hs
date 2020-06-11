{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.File
  ( postUploadR
  , getUploadFileR
  , deleteUploadFileR
  )
where

import           Import
import           System.Directory

postUploadR :: UploadTarget -> Handler Value
postUploadR target = do
  app  <- getYesod
  info <- runInputPost $ ireq fileField "file"
  let filePath = unpack (fileName info)
  let name = case target of
        UploadBin        -> binFile app filePath
        UploadSrc        -> srcFile app filePath
        UploadScreenshot -> screenshotFile app filePath

  exists <- liftIO $ doesFileExist name
  if exists
    then pure $ object

      ["name" .= fileName info, "error" .= ("File already exists" :: Text)]
    else do
      liftIO $ fileMove info name
      render <- getUrlRender

      pure $ object
        [ "name" .= fileName info
        , "deleteUrl" .= render (UploadFileR target (fileName info))
        ]

getUploadFileR :: UploadTarget -> Text -> Handler Html
getUploadFileR target filename = do
  app <- getYesod
  let realFileName = case target of
        UploadBin        -> binFile app (unpack filename)
        UploadSrc        -> srcFile app (unpack filename)
        UploadScreenshot -> screenshotFile app (unpack filename)
  exists <- liftIO (doesFileExist realFileName)
  if exists then sendFile typeOctet realFileName else notFound

deleteUploadFileR :: UploadTarget -> Text -> Handler Value
deleteUploadFileR target filename = do
  app <- getYesod
  let filePath = case target of
        UploadBin        -> binFile app (unpack filename)
        UploadSrc        -> srcFile app (unpack filename)
        UploadScreenshot -> screenshotFile app (unpack filename)
  liftIO $ removeFile filePath
  pure $ object ["files" .= [object [filename .= True]]]
