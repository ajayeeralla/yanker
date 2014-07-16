{-# LANGUAGE DeriveGeneric #-}
module SemanticScheme where

import GHC.Generics
import Data.Binary
import Data.Set as Set
import System.IO.Error

import TypeHierarchy
import OpenGraph
import GraphPresentation


-- The type of an entry in the skeleton list,
-- storing the skeleton itself, the associated graph,
-- and its presentation.
data SkelEntry = SkelEntry {
    skeleton :: LambekSkel,
    skelGraph :: OGraph,
    skelPres :: GraphPresentation }
   deriving (Eq,Show,Generic)

instance Binary SkelEntry
    
-- A semantic scheme is an ordered list of type skeletons (polymorphic lambek
-- types), with a corresponding open graph for each of them.
-- For the purpose of editing, a graph presentation can also be stored.
type SemanticScheme = [SkelEntry]

-- Load a semantic scheme from a file
loadSemanticScheme :: String -> IO (Either String SemanticScheme)
loadSemanticScheme fileName =
  catchIOError
      (do
          s <- decodeFile $ fileName
          return $ Right s)
      (\error -> return $ Left (show error))

-- Save a semantic scheme to a file
saveSemanticScheme :: FilePath -> SemanticScheme -> IO ()
saveSemanticScheme fileName semScheme =
  encodeFile fileName semScheme

-- Create an empty graph corresponding to a Lambek Skeleton
emptyGraphFromSkel :: LambekSkel -> OGraph
emptyGraphFromSkel sk =
  OGraph (reverse gates) [] (Set.empty)
  where
    (gates,_) = dfs [] 1 False sk
    dfs :: [OGate] -> Int -> Bool -> LambekSkel -> ([OGate],Int)
    dfs accu count productive (LSAtom base _) =
      ((OGate (base ++ (show count)) productive):accu, count+1)
    dfs accu count productive (LSVar name) =
      ((OGate ((show name) ++ "-" ++ (show count)) productive):accu, count+1)
    dfs accu count productive (LSLeft body argument) =
      let (accu2,count2) = dfs accu count (not productive) argument in
      dfs accu2 count2 productive body
    dfs accu count productive (LSRight body argument) =
      let (accu2,count2) = dfs accu count productive body in
      dfs accu2 count2 (not productive) argument

-- Create a default SkelEntry from a skeleton
defaultSkelEntry :: LambekSkel -> SkelEntry
defaultSkelEntry skel =
  SkelEntry skel (emptyGraphFromSkel skel) emptyGraphPresentation

-- Default semantic scheme: only one skeleton, matching everything.
defaultSemanticScheme :: SemanticScheme
defaultSemanticScheme =
  [SkelEntry defaultSkel (emptyGraphFromSkel defaultSkel) emptyGraphPresentation]
  where
    defaultSkel = LSVar 1

-- Get the nth element of the scheme, if any
getNthSkeleton :: SemanticScheme -> Int -> Maybe SkelEntry
getNthSkeleton lst idx =
  if idx >= 0 && idx < length lst then
    Just (lst !! idx)
  else
    Nothing
    
-- TODO: combine it with parsing.