{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Handler.File where

import Import
import System.Directory

postUploadR :: UploadTarget -> Handler Value
postUploadR target = do
  info <- runInputPost$ ireq fileField "file"
  let filePath = unpack$ fileName info
  let name = case target of
               UploadBin -> binFile filePath
               UploadSrc -> srcFile filePath
               UploadScreenshot -> screenshotFile filePath

  exists <- liftIO$ doesFileExist name
  if exists then
    return$ object ["files" .= [
      object [
        "name" .= fileName info,
        "error" .= ("File already exists" :: Text)]]]
  else do
    size <- liftIO$ do
      fileMove info name
      getFileSize name

    render <- getUrlRender

    return$ object ["files" .= [
      object [
        "name" .= fileName info,
        "size" .= size,
        "url" .= (render$ UploadFileR target (fileName info)),
        "deleteUrl" .= (render$ UploadFileR target (fileName info)),
        "deleteType" .= ("DELETE" :: Text)
      ]]]

getUploadFileR :: UploadTarget -> Text -> Handler Html
getUploadFileR target filename =
  case target of
    UploadBin -> sendFile typeOctet (binFile$ unpack filename)
    UploadSrc -> sendFile typeOctet (srcFile$ unpack filename)
    UploadScreenshot -> sendFile typeOctet (screenshotFile$ unpack filename)

deleteUploadFileR :: UploadTarget -> Text -> Handler Value
deleteUploadFileR target filename = do
  let filePath = case target of
               UploadBin -> binFile (unpack filename)
               UploadSrc -> srcFile (unpack filename)
               UploadScreenshot -> screenshotFile (unpack filename)
  liftIO$ removeFile filePath
  return$ object ["files" .= [object [filename .= True]]]

