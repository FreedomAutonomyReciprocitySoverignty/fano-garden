{-# LANGUAGE OverloadedStrings #-}

module ULP.NDJSON
  ( decodeFile
  , encodeLine
  , appendLine
  ) where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as LBS
import ULP.Types

encodeLine :: CommitEvent -> LBS.ByteString
encodeLine = (`LBS.append` "\n") . A.encode

decodeFile :: FilePath -> IO [CommitEvent]
decodeFile fp = do
  content <- LBS.readFile fp
  pure $ mapMaybeDecode (LBS.lines content)

mapMaybeDecode :: [LBS.ByteString] -> [CommitEvent]
mapMaybeDecode = foldr step []
  where
    step line acc =
      case A.eitherDecode line of
        Left _ -> acc
        Right x -> x : acc

appendLine :: FilePath -> CommitEvent -> IO ()
appendLine fp c = LBS.appendFile fp (encodeLine c)
