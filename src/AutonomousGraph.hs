{-# Language DeriveGeneric #-}

import Data.Maybe
import Data.List
import qualified Data.Set as Set
import GHC.Generics as GGen
import Utilities

---------------------------------
-- Types for autonomous graphs --
---------------------------------

-- Basic types
type ABType = String

-- Producer or consumer?
data AProdCons =
    AProd
  | ACons
  deriving (Eq,Ord,Show,GGen.Generic)

-- A type is a basic type and a boolean indicating
-- whether it produces or consumes
data AType = AType ABType AProdCons
  deriving (Eq,Ord,Show,GGen.Generic)

-- Domain or codomain ?
data ADomCod =
    ADom
 | ACod
  deriving (Eq,Ord,Show,GGen.Generic)

-- Designates the position of a gate in a graph
data APath =
   APBoundary ADomCod Int
 | APNode Int ADomCod Int
  deriving (Eq,Ord,Show,GGen.Generic)
   
-- An edge between two paths
data AEdge = AEdge APath APath
  deriving (Eq,Ord,Show,GGen.Generic)

-- A typed node
data ANode = ANode {
  aDom :: [AType],
  aCod :: [AType],
  aDat :: String }
  deriving (Eq,Ord,Show,GGen.Generic)

-- An autonomous graph
data AGraph = AGraph {
  aGDom :: [AType],
  aGCod :: [AType],
  aNodes :: [(Int,ANode)],
  aEdges :: (Set.Set AEdge) }
  deriving (Eq,Ord,Show,GGen.Generic)

---------------
-- Utilities --
---------------

getANode :: AGraph -> Int -> Maybe ANode
getANode graph nodeId =
  find (\ (nid,_) -> nid == nodeId) (aNodes graph) >>=
  (\ (nid,val) -> return val)

getATypeByAPath :: AGraph -> APath -> Maybe AType
getATypeByAPath graph@(AGraph dom cod nodes edges) path =
  case path of
    APBoundary ADom x -> nth cod x
    APBoundary ACod x -> nth dom x
    APNode nid domCod x -> do
       (ANode dom cod _) <- getANode graph nid
       (case domCod of
          ADom -> nth dom x
          ACod -> nth cod x)
        

---------------------------
-- Graph Sanity Checking --
---------------------------

validAutonomous graph@(AGraph dom cod nodes edges) =
  allSet isValidEdge edges &&
  all (gatesInPathSet pathSet) nodes &&
  True -- TODO: boundary gates in path set
  where
    isValidEdge (AEdge from to) =
      from /= to &&
      typeFrom /= Nothing &&
      typeFrom == typeTo
      where
        typeFrom = getATypeByAPath graph from
        typeTo = getATypeByAPath graph to
    gatesInPathSet pathSet (nodeId,(ANode dom cod _)) =
      Set.isSubsetOf localPathSet pathSet
      where
        localPathSet = Set.union domPathSet codPathSet
        domPathSet = typeToPathSet (APNode nodeId ADom) dom
        codPathSet = typeToPathSet (APNode nodeId ACod) cod
        typeToPathSet label lst =
          Set.fromList $ map label $ seqInt (length lst) []
    pathSet =
      Set.unions (map
                  (\ (AEdge u v) -> Set.fromList [u,v])
                  $ Set.elems edges)
    allNodes = []