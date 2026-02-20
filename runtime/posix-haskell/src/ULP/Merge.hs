{-# LANGUAGE OverloadedStrings #-}

module ULP.Merge
  ( mergeCommits
  , chooseTip
  ) where

import Data.Function (on)
import Data.List (maximumBy, nubBy, sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import ULP.Types

commitKey :: CommitEvent -> Text
commitKey = self_hash

dedupeByHash :: [CommitEvent] -> [CommitEvent]
dedupeByHash = nubBy ((==) `on` commitKey)

lcOf :: CommitEvent -> Int
lcOf c = maybe (-1) id (lc c)

compareCommit :: CommitEvent -> CommitEvent -> Ordering
compareCommit a b =
  compare (lcOf a) (lcOf b)
    <> compare (t a) (t b)
    <> compare (cid a) (cid b)

mergeCommits :: [CommitEvent] -> [CommitEvent] -> [CommitEvent]
mergeCommits local remote = sortBy compareCommit (dedupeByHash (local ++ remote))

chooseTip :: [CommitEvent] -> Maybe CommitEvent
chooseTip [] = Nothing
chooseTip commits =
  let sealed = filter (== Sealed) (map cstatus commits)
      pool = if null sealed then commits else filter ((== Sealed) . cstatus) commits
   in Just $ maximumBy compareTip pool

compareTip :: CommitEvent -> CommitEvent -> Ordering
compareTip a b =
  compare (lcOf a) (lcOf b)
    <> compare (t a) (t b)
    <> compare (T.unpack (self_hash b)) (T.unpack (self_hash a))
