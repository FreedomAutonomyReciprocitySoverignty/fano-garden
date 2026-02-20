{-# LANGUAGE OverloadedStrings #-}

module ULP.Merkle
  ( computeCommitMerkle
  , validateCommitMerkle
  , getSigningMessage
  ) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import ULP.Canonical
import ULP.Types

pairHash :: Text -> Text -> Text
pairHash a b = sha256Hex (BSC.pack (T.unpack a ++ "|" ++ T.unpack b))

merkleRoot :: [Text] -> Text
merkleRoot [] = sha256Hex "empty"
merkleRoot [x] = x
merkleRoot xs = merkleRoot (go xs)
  where
    go [] = []
    go [x] = [pairHash x x]
    go (a:b:rest) = pairHash a b : go rest

sectionHash :: ToJSON a => a -> Text
sectionHash = sha256Hex . stableJson . toJSON

computeCommitMerkle :: CommitEvent -> Merkle
computeCommitMerkle c =
  let identitiesHash = sectionHash (fromMaybe [] (identities c))
      vertexHash = sectionHash (vertex c)
      edgesHash = sectionHash (edges c)
      facesHash = sectionHash (faces c)
      centroidHash = sectionHash (centroid c)
      metaObj =
        case lc c of
          Nothing ->
            object
              [ "cid" .= cid c
              , "t" .= t c
              , "ctype" .= ctype c
              , "parents" .= parents c
              , "cstatus" .= cstatus c
              , "prev_hash" .= prev_hash c
              ]
          Just n ->
            object
              [ "cid" .= cid c
              , "t" .= t c
              , "lc" .= n
              , "ctype" .= ctype c
              , "parents" .= parents c
              , "cstatus" .= cstatus c
              , "prev_hash" .= prev_hash c
              ]
      metaHash = sha256Hex (stableJson metaObj)
      order = ["identities_hash", "vertex_hash", "edges_hash", "faces_hash", "centroid_hash", "meta_hash"]
      sectionPairs =
        [ ("identities_hash", identitiesHash)
        , ("vertex_hash", vertexHash)
        , ("edges_hash", edgesHash)
        , ("faces_hash", facesHash)
        , ("centroid_hash", centroidHash)
        , ("meta_hash", metaHash)
        ]
      rootHash = merkleRoot (map snd sectionPairs)
   in Merkle {version = "v1", sections = sectionPairs, leaf_order = order, root = rootHash}

validateCommitMerkle :: CommitEvent -> Bool
validateCommitMerkle c =
  case merkle c of
    Nothing -> True
    Just m ->
      let expected = computeCommitMerkle c
       in version m == version expected
            && leaf_order m == leaf_order expected
            && sections m == sections expected
            && root m == root expected

getSigningMessage :: CommitEvent -> Text
getSigningMessage c = maybe (self_hash c) root (merkle c)
