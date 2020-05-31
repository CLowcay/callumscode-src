{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types
  ( Year
  , Month
  , Markdown(..)
  , mkYear
  , mkMonth
  , mkMonthS
  , SoftwareCategory(..)
  , UploadTarget(..)
  )
where

import           CMark
import           ClassyPrelude.Yesod
import           Database.Persist.Sql
import           Text.Blaze

newtype Markdown = Markdown { unMarkdown :: Text}
instance PersistField Markdown where
  toPersistValue (Markdown x) = PersistText x
  fromPersistValue (PersistText x) = Right (Markdown x)
  fromPersistValue x               = Left $ "Invalid markdown " ++ pack (show x)
instance PersistFieldSql Markdown where
  sqlType _ = SqlString
instance ToMarkup Markdown where
  toMarkup (Markdown x) =
    preEscapedToMarkup (commonmarkToHtml [optSafe, optSmart] x)

data SoftwareCategory = Haskell | XBlite | LibertyBasic
  deriving (Show, Read, Eq)

data UploadTarget = UploadBin | UploadSrc | UploadScreenshot
  deriving (Show, Read, Eq)

instance PathPiece UploadTarget where
  toPathPiece   = tshow
  fromPathPiece = readMay

derivePersistField "SoftwareCategory"

newtype Year = Year Int deriving (Read, Eq, Ord)
newtype Month = Month Int deriving (Read, Eq, Ord)

instance Show Year where
  show (Year x) = show x

mkYear :: Int -> Maybe Year
mkYear x | x == 0    = Nothing
         | otherwise = Just $ Year x

mkMonth :: Int -> Maybe Month
mkMonth x | x >= 0 && x < 12 = Just $ Month x
          | otherwise        = Nothing

mkMonthS :: Text -> Maybe Month
mkMonthS "jan" = Just $ Month 0
mkMonthS "feb" = Just $ Month 1
mkMonthS "mar" = Just $ Month 2
mkMonthS "apr" = Just $ Month 3
mkMonthS "may" = Just $ Month 4
mkMonthS "jun" = Just $ Month 5
mkMonthS "jul" = Just $ Month 6
mkMonthS "aug" = Just $ Month 7
mkMonthS "sep" = Just $ Month 8
mkMonthS "oct" = Just $ Month 9
mkMonthS "nov" = Just $ Month 10
mkMonthS "dec" = Just $ Month 11
mkMonthS _     = Nothing

instance Show Month where
  show (Month 0 ) = "jan"
  show (Month 1 ) = "feb"
  show (Month 2 ) = "mar"
  show (Month 3 ) = "apr"
  show (Month 4 ) = "may"
  show (Month 5 ) = "jun"
  show (Month 6 ) = "jul"
  show (Month 7 ) = "aug"
  show (Month 8 ) = "sep"
  show (Month 9 ) = "oct"
  show (Month 10) = "nov"
  show (Month 11) = "dec"
  show _          = error "Invalid month value!"

instance PathPiece Year where
  fromPathPiece = (mkYear =<<) . readMay
  toPathPiece   = pack . show

instance PathPiece Month where
  fromPathPiece = mkMonthS
  toPathPiece   = pack . show

instance PersistField Year where
  toPersistValue (Year x) = PersistInt64 . fromIntegral $ x
  fromPersistValue (PersistInt64 x) = case mkYear . fromIntegral $ x of
    Nothing -> Left $ "Invalid year " ++ pack (show x)
    Just v  -> Right v
  fromPersistValue x = Left $ "Invalid year " ++ pack (show x)

instance PersistField Month where
  toPersistValue (Month x) = PersistInt64 . fromIntegral $ x
  fromPersistValue (PersistInt64 x) = case mkMonth . fromIntegral $ x of
    Nothing -> Left $ "Invalid month " ++ pack (show x)
    Just v  -> Right v
  fromPersistValue x = Left $ "Invalid month " ++ pack (show x)

instance PersistFieldSql Year where
  sqlType _ = SqlInt64

instance PersistFieldSql Month where
  sqlType _ = SqlInt64
