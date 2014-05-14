module OpenGraph where

import Data.List
import qualified Data.Set as Set

-- Internal identifiers (pointers) for nodes
type OId = Int
boundaryId = 0

-- Gate in a node of a graph: produces or consumes an entity. True when it produces
data OGate = OGate String Bool
   deriving (Eq)
-- Negates a gate
neg (OGate s b) = OGate s (not b)
-- Selects only producer (resp. consumer) gates in a list
producers = filter (\ (OGate _ b) -> b)
consumers = filter (\ (OGate _ b) -> not b)

-- Node in a graph: a name (not used internally) and a bunch of gates
data ONode = ONode String [OGate]
   deriving (Eq)
-- Turns a node upside down: negates its gates
flip (ONode name gates) = ONode name (map neg gates)

-- Path in a graph: the identifier of a node plus the name of the corresponding gate
data OPath = OPath OId String
   deriving (Eq,Ord)

-- Edge in a graph: path to a producer and path to a consumer
data OEdge = OEdge OPath OPath
   deriving (Eq,Ord)

-- Open graph: a list of gates as boundaries, a list of nodes, and a list of edges
data OGraph = OGraph [OGate] [(OId,ONode)] (Set.Set OEdge)
   deriving (Eq)
-- All the nodes, including a fake node for the boundary
nodesAndBoundary (OGraph boundary nodes _) =
   (boundaryId,(ONode "" boundary)):nodes

-- Get the polarity of a gate (if it exists)
getGate (OPath nodeId gateName) nodes =
   (find (\ (id,_) -> id == nodeId) nodes)
   >>= (\ (_,(ONode _ gates)) -> find (\ (OGate s b) -> s == gateName) gates)
   >>= (\ (OGate _ b) -> Just b)

allSet pred = Set.foldl' (\ found elem -> found && pred elem) True
concatSet = Set.unions . Set.elems

-- Check that a graph is valid, i.e. that:
-- 1/ all edges are linked to valid nodes
-- 2/ all producer gates are linked to at least one consumer gate
-- 3/ all consumer gates are linked to exactly one producer gate
checkGraph g@(OGraph boundary nodes edges) =
    allSet isValidEdge edges &&
    all (\ (nodeId,(ONode _ gates)) ->
          all (\ (OGate gateName productive) ->
                hasEdge (OPath nodeId gateName) productive)
              gates)
        allNodes
    where
      isValidEdge (OEdge from to) =
        (polarityFrom >>= (return . not)) == polarityTo
        where polarityFrom = getGate from allNodes
              polarityTo = getGate to allNodes
      hasEdge path productive =
        if productive then
           countSet (\ (OEdge from to) -> from == path) edges >= 1
        else
           countSet (\ (OEdge from to) -> to == path) edges == 1
      allNodes = nodesAndBoundary g
      countSet pred = Set.foldl' (\ cnt elem -> if pred elem then cnt+1 else cnt) 0
      
-- Get the maximum id attributed to a node
maxOId nodesList =
  maximum (0:(map fst nodesList))

-- Shift the indices of a graph (but the boundary)
shiftGraph offset (OGraph bound nodes edges) =
  OGraph bound newNodes newEdges
  where
      translateId id = if id == boundaryId then id else id + offset
      translatePath (OPath id gate) = OPath (translateId id) gate
      translateEdge (OEdge from to) = OEdge (translatePath from) (translatePath to)
      translateNode (id,node) = (translateId id,node)
      newNodes = map translateNode nodes
      newEdges = Set.map translateEdge edges

-- Vertical composition of two open graphs (if defined)
(vertComp) (OGraph boundA nodesA edgesA) b@(OGraph boundB nodesB edgesB) =
  if producers boundA /= consumers boundB then
    Nothing
  else
    Just (OGraph newBound newNodes newEdges) -- newNodes newEdges
    where
      OGraph _ nodesBShifted edgesBShifted = shiftGraph (maxOId nodesA) b
      newBound = (consumers boundA) ++ (producers boundB)
      newEdgesB = concatSet . Set.map (joinPath edgesA False) $ edgesBShifted
      newEdgesA = concatSet . Set.map (joinPath edgesB True) $ edgesA
      newEdges = Set.union newEdgesA newEdgesB
      newNodes = nodesA ++ nodesBShifted
      joinPath otherEdges productive edge = case edge of
        -- fromPath -> fromGate -> to
        OEdge fromGate@(OPath boundaryId _) to | (not productive) ->
          Set.map (\ (OEdge fromPath _) -> OEdge fromPath to)
              (Set.filter (\ (OEdge _ eTo) -> eTo == fromGate) otherEdges)
        -- from -> toGate -> toPath
        OEdge from toGate@(OPath boundaryId _) | productive ->
          Set.map (\ (OEdge _ toPath) -> OEdge from toPath)
              (Set.filter (\ (OEdge eFrom _) -> eFrom == toGate) otherEdges)
        another -> Set.singleton another

-- Horizontal composition of two open graphs (always defined)
(horiComp) (OGraph boundA nodesA edgesA) (OGraph boundB nodesB edgesB) =
  OGraph bound nodes edges
  where
    bound = (map (\ (OGate name b) -> (OGate ("0"++name) b)) boundA) ++
            (map (\ (OGate name b) -> (OGate ("1"++name) b)) boundB)
    nodes = nodesA ++ nodesB
    edges = Set.union (Set.map (translateEdge "0") edgesA) (Set.map (translateEdge "1") edgesB)
    translateEdge prefix (OEdge from to) = OEdge (translatePath prefix from)
                                                 (translatePath prefix to)
    translatePath prefix (OPath id name) =
       if id == boundaryId then
          OPath id (prefix ++ name)
       else
          OPath id name

-- Outputs the graph to dotty
toDotty (OGraph bound nodes edges) =
   "/* Output generated by http://github.com/wetneb/yanker */\ndigraph G {\n" ++
   (Set.foldl (\ accu (OEdge from to) -> "\"" ++ (pPath from) ++ "\" -> \"" ++ (pPath to) ++"\";\n") "" edges) ++
   "}\n"
   where
    pPath (OPath id gate) = show id


      
           