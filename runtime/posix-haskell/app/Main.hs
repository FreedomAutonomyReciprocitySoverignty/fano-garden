{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Int (Int64)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)
import System.FilePath ((</>))
import ULP.Merge (chooseTip, mergeCommits)
import ULP.NDJSON (decodeFile)
import ULP.Runtime
import ULP.Storage (ensureRoot, loadLog, logPath)
import ULP.Types

nowMs :: IO Int64
nowMs = round . (* 1000) <$> getPOSIXTime

defaultOptions :: FilePath -> RuntimeOptions
defaultOptions root =
  RuntimeOptions
    { clock = nowMs
    , counterStart = 0
    , signer = Nothing
    , verifier = Nothing
    , storageRoot = root
    }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["init", "--root", root] -> do
      ensureRoot root
      putStrLn ("initialized: " ++ root)

    ["commit", "--root", root, "--type", _ty] -> do
      rt <- initRuntime (defaultOptions root)
      c <- appendRuntimeCommit rt Commit
      putStrLn ("commit: " ++ show (cid c))

    ["validate", "--root", root] -> do
      rt <- initRuntime (defaultOptions root)
      rs <- validateLog rt (logPath root)
      putStrLn ("validated records: " ++ show (length rs))
      putStrLn ("valid count: " ++ show (length (filter valid rs)))

    ["merge", "--root", root, "--from", remote] -> do
      local <- loadLog root
      remoteC <- decodeFile remote
      let merged = mergeCommits local remoteC
      putStrLn ("merged commits: " ++ show (length merged))
      case chooseTip merged of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["tip", "--root", root] -> do
      cs <- loadLog root
      case chooseTip cs of
        Nothing -> putStrLn "tip: none"
        Just c -> putStrLn ("tip: " ++ show (cid c))

    ["replay", "--root", root] -> do
      cs <- loadLog root
      putStrLn ("replayed commits: " ++ show (length cs))

    _ -> do
      putStrLn "Usage:"
      putStrLn "  ulp-runtime init --root <dir>"
      putStrLn "  ulp-runtime commit --root <dir> --type commit"
      putStrLn "  ulp-runtime validate --root <dir>"
      putStrLn "  ulp-runtime merge --root <dir> --from <remote.ndjson>"
      putStrLn "  ulp-runtime tip --root <dir>"
      putStrLn "  ulp-runtime replay --root <dir>"
