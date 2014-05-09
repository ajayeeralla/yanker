
import Data.List

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
   deriving (Eq)

-- Edge in a graph: path to a producer and path to a consumer
data OEdge = OEdge OPath OPath
   deriving (Eq)

-- Open graph: a list of gates as boundaries, a list of nodes, and a list of edges
data OGraph = OGraph [OGate] [(OId,ONode)] [OEdge]
   deriving (Eq)
-- All the nodes, including a fake node for the boundary
nodesAndBoundary (OGraph boundary nodes _) =
   (boundaryId,(ONode "" boundary)):nodes

-- Get the polarity of a gate (if it exists)
getGate (OPath nodeId gateName) nodes =
   (find (\ (id,_) -> id == nodeId) nodes)
   >>= (\ (_,(ONode _ gates)) -> find (\ (OGate s b) -> s == gateName) gates)
   >>= (\ (OGate _ b) -> Just b)

-- Check that a graph is valid, i.e. that:
-- 1/ all edges are linked to valid nodes
-- 2/ all producer gates are linked to at least one consumer gate
-- 3/ all consumer gates are linked to exactly one producer gate
checkGraph g@(OGraph boundary nodes edges) =
    all isValidEdge edges &&
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
           count (\ (OEdge from to) -> from == path) edges >= 1
        else
           count (\ (OEdge from to) -> to == path) edges == 1
      allNodes = nodesAndBoundary g
      count predicate =
        foldl (\ accu e -> if predicate e then accu+1 else accu) 0

-- Vertical composition of two open graphs (if defined)
(vertComp) (OGraph boundA nodesA edgesA) (OGraph boundB nodesB edgesB) =
  if producers boundA /= consumers boundB then
    Nothing
  else
    Just (OGraph newBound [] []) -- newNodes newEdges
    where
      newBound = (consumers boundA) ++ (producers boundB)
      translateId id = id + (length nodesA)
      joinPath edge otherEdges productive = case edge of
        -- fromPath -> fromGate -> to
        OEdge fromGate@(OPath boundaryId _) to | (not productive) ->
          map (\ (OEdge fromPath _) -> OEdge fromPath to)
              (filter (\ (OEdge _ eTo) -> eTo == fromGate) otherEdges)
        -- from -> toGate -> toPath
        OEdge from toGate@(OPath boundaryId _) | productive ->
          map (\ (OEdge _ toPath) -> OEdge from toPath)
              (filter (\ (OEdge eFrom _) -> eFrom == toGate) otherEdges)
        another -> [another] 

         
      
           