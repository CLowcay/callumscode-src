{-# LANGUAGE NoImplicitPrelude #-}
module Common
  ( mkURL
  , adjustURL
  )
where

import           ClassyPrelude.Yesod     hiding ( toLower )
import           Data.Char                      ( isSpace
                                                , toLower
                                                )

mkURL :: Text -> Text
mkURL =
  pack
    . fmap (formatSpace '-')
    . concatMap (take 1)
    . groupBy (\a b -> isSpace a && isSpace b)
    . fmap toLower
    . unpack

adjustURL :: Text -> Text -> Text
adjustURL lastURL thisURL = thisURL <> pad 4 '0' (add1 $ suffix 4 lastURL)
  where add1 = tshow . (+ 1) . fromMaybe (0 :: Int) . readMay

suffix :: Int -> Text -> Text
suffix w = reverse . take w . reverse

pad :: Int -> Char -> Text -> Text
pad w c = reverse . pack . take w . (<> repeat c) . unpack . reverse

formatSpace :: Char -> Char -> Char
formatSpace repl x = if isSpace x then repl else x

