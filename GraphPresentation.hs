{-# LANGUAGE DeriveGeneric #-}

module GraphPresentation where

import qualified Data.Map.Strict as Map
import Data.List
import GHC.Generics as GGen
import OpenGraph

-- Defines the positions of the nodes of a graph onscreen
type GraphPresentation = Map.Map OId (Double,Double)

-- Bounding box for nodes and gates
data BoundingBox = BBox Double Double Double Double
     deriving (Eq,Ord,Show,GGen.Generic)


-- Drawing constants
nodeSemiHeight = 20.0
nodeGateSpacing = 15.0
nodeGateVertOffset = 10.0
gateRadius = 3.0
gateBBhalfSize = 5.0

-- Where to draw outer gates
outerGateOffset = 60.0
outerGateSpacing = 60.0
topOuterGates = 60.0
bottomOuterGates = 400.0
gateLineEnd = 460.0

-- Where to draw the type
typeSkeletonPosition = 30.0

-- Get the position of a gate designated by a path
-- given a graph and a graph presentation
getGatePos :: OGraph -> GraphPresentation -> OPath -> (Double,Double)
getGatePos (OGraph gates nodes _) presentation (OPath id name) =
    if id == boundaryId then
       let Just (pos,producer) = findGateNameBoundary name gates in
       (outerGateOffset + outerGateSpacing*pos,
       if producer then topOuterGates else bottomOuterGates)
    else
       let Just (_,(ONode _ gates)) = find (\ (cid,_) -> cid==id) nodes in
       let Just (pos,producer) = findGateName name gates in
       let Just (posX,posY) = Map.lookup id presentation in
       let nInputs = fromIntegral . length . consumers $ gates in
       let nOutputs = fromIntegral . length . producers $ gates in
       let topWidth = (nInputs+1)*nodeGateSpacing in
       let bottomWidth = (nOutputs+1)*nodeGateSpacing in
       if producer then
          (posX-(bottomWidth/2)+nodeGateSpacing*(pos+1),posY+nodeSemiHeight)
       else
          (posX-(topWidth/2)+nodeGateSpacing*(pos+1), posY-nodeSemiHeight)


-- Create a bounding box for a node given the number
-- of inputs, outputs and its position
makeNodeBoundingBox :: Int -> Int -> Double -> Double -> BoundingBox
makeNodeBoundingBox 0 0 x y =
    BBox (x-nodeSemiHeight) (y-nodeSemiHeight) (2*nodeSemiHeight) (2*nodeSemiHeight)

makeNodeBoundingBox nInputs nOutputs x y =
    let tot = max nInputs nOutputs in
    let width = (fromIntegral (tot+1))*nodeGateSpacing in
    BBox (x - width/2) (y - nodeSemiHeight) width (2*nodeSemiHeight)

boundingBoxFromNode presentation (id,(ONode _ gates)) =
    makeNodeBoundingBox
      (length (consumers gates))
      (length (producers gates))
      x
      y
    where Just (x,y) = Map.lookup id presentation


boundingBoxFromGate graph presentation (OPath nid name) =
    let (x,y) = getGatePos graph presentation (OPath nid name) in
    BBox (x-gateBBhalfSize) (y-gateBBhalfSize) (2*gateBBhalfSize) (2*gateBBhalfSize)

-- Is this point in the bounding box ?
inBoundingBox :: Double -> Double -> BoundingBox -> Bool
inBoundingBox x y (BBox startx starty width height) =
    (x >= startx && y >= starty && x <= startx+width && y <= starty + height)

-- If it is in the bounding box, is it in the lower part ?
inLowerPartOfBBox x y (BBox _ starty _ height) =
    y >= starty + height/2

-- Get the key of a bounding box containing the point (if any)
findBoundingBox x y =
    Map.foldlWithKey (\ fnd id bb -> if (inBoundingBox x y bb) then (Just id) else fnd) Nothing
