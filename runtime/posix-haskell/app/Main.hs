{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)
import ULP.Merge (chooseTip, mergeCommits)
import ULP.NDJSON (decodeFile)
import ULP.Runtime
import ULP.Storage (ensureRoot, loadLog, logPath, writeLog)
import ULP.Types

nowMs :: IO Int64
nowMs = round . (* 1000) <$> getPOSIXTime

defaultOptions :: FilePath -> RuntimeOptions
defaultOptions rootDir =
  RuntimeOptions
    { clock = nowMs
    , counterStart = 0
    , signer = Nothing
    , verifier = Nothing
    , storageRoot = rootDir
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init", "--root", rootDir] -> do
      ensureRoot rootDir
      putStrLn ("initialized: " ++ rootDir)

    ["commit", "--root", rootDir, "--type", _ty] -> do
      rt <- initRuntime (defaultOptions rootDir)
      c <- appendRuntimeCommit rt Commit
      putStrLn ("commit: " ++ show (cid c))

    ["validate", "--root", rootDir] -> do
      rt <- initRuntime (defaultOptions rootDir)
      rs <- validateLog rt (logPath rootDir)
      putStrLn ("validated records: " ++ show (length rs))
      putStrLn ("valid count: " ++ show (length (filter valid rs)))

    ["merge", "--root", rootDir, "--from", remote] -> do
      local <- loadLog rootDir
      remoteC <- decodeFile remote
      let merged = mergeCommits local remoteC
      writeLog rootDir merged
      putStrLn ("merged commits: " ++ show (length merged))
      case chooseTip merged of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["tip", "--root", rootDir] -> do
      cs <- loadLog rootDir
      case chooseTip cs of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["replay", "--root", rootDir] -> do
      cs <- loadLog rootDir
      putStrLn ("replayed commits: " ++ show (length cs))

    _ -> do
      putStrLn "Usage:"
      putStrLn "  ulp-runtime init --root <dir>"
      putStrLn "  ulp-runtime commit --root <dir> --type commit"
      putStrLn "  ulp-runtime validate --root <dir>"
      putStrLn "  ulp-runtime merge --root <dir> --from <remote.ndjson>"
      putStrLn "  ulp-runtime tip --root <dir>"
      putStrLn "  ulp-runtime replay --root <dir>"
