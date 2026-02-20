{-# LANGUAGE OverloadedStrings #-}

module ULP.Storage
  ( ensureRoot
  , logPath
  , loadLog
  , appendCommit
  ) where

import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import ULP.NDJSON
import ULP.Types

ensureRoot :: FilePath -> IO ()
ensureRoot root = createDirectoryIfMissing True root

logPath :: FilePath -> FilePath
logPath root = root </> "log.ndjson"

loadLog :: FilePath -> IO [CommitEvent]
loadLog root = do
  let fp = logPath root
  exists <- doesFileExist fp
  if exists then decodeFile fp else pure []

appendCommit :: FilePath -> CommitEvent -> IO ()
appendCommit root c = appendLine (logPath root) c
