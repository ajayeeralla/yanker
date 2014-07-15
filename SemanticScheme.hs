{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Binary
import System.IO.Error

import TypeHierarchy
import OpenGraph
import GraphPresentation

-- A semantic scheme is an ordered list of type skeletons (polymorphic lambek
-- types), with a corresponding open graph for each of them.

-- For the purpose of editing, a graph presentation can also be stored.

data SemanticScheme = SScheme [(LambekSkel,OGraph,GraphPresentation)]
     deriving (Eq,Show,Generic)

instance Binary SemanticScheme

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

-- TODO: combine it with parsing.