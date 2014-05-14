module DrawGraph where

-- Data structures
import Data.List as List
import Data.Set as Set
import qualified Data.Map.Strict as Map
import OpenGraph as OG

-- GUI / Rendering stuff
import Graphics.Rendering.Cairo
import Control.Concurrent.MVar
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Abstract.Widget

-- Defines the positions of the nodes of a graph onscreen
type GraphPresentation = Map.Map OId (Double,Double)

-- Current state of the pointer
data DrawingState = DSelect | DMoving OId | DNode | DEdge | DDrawing OPath
     deriving (Eq)

-- Bounding box for nodes and gates
data BoundingBox = BBox Double Double Double Double
     deriving (Eq,Ord)

-- Current state of the object selection
data ElemSelection = SelectNode OId | SelectEdge OPath | NoSelection

-- Global state of the graph editor
data GraphState = GraphState {
       totalGraph :: OGraph,
       presentation :: GraphPresentation,
       selection :: ElemSelection,
       nodeBB :: (Set.Set (OId,BoundingBox)),
       gateBB :: (Set.Set (OPath,BoundingBox)) }

-- Drawing constants
nodeSemiHeight = 20
nodeGateSpacing = 15
nodeGateVertOffset = 10
gateRadius = 3

-- Where to draw outer gates
outerGateOffset = 60
outerGateSpacing = 60
topOuterGates = 60
bottomOuterGates = 400

seqInt 0 accu = accu
seqInt n accu = seqInt (n-1) (n:accu)

doList f = List.foldl (\ accu elem -> accu >> (f elem)) (return ())

-- Draw a node onscreen
drawNode :: Double -> Double -> Int -> Int -> Render ()
drawNode posX posY 0 0 = do
    setSourceRGB 0 0 0
    setLineWidth 1
    
    moveTo posX posY
    relMoveTo 0 (-nodeSemiHeight)
    relLineTo nodeSemiHeight nodeSemiHeight
    relLineTo (-nodeSemiHeight) nodeSemiHeight
    relLineTo (-nodeSemiHeight) (-nodeSemiHeight)
    relLineTo (2*nodeSemiHeight) 0
    relMoveTo (-2*nodeSemiHeight) 0
    relLineTo nodeSemiHeight (-nodeSemiHeight)

    stroke
  
drawNode posX posY nInputs nOutputs = do
    setSourceRGB 0 0 0
    setLineWidth 1
    let height = (nodeSemiHeight*2)
    let topWidth = ((fromIntegral nInputs+1)*nodeGateSpacing)
    let bottomWidth = ((fromIntegral nOutputs+1)*nodeGateSpacing)

    moveTo posX posY
    relMoveTo (topWidth/2) (-nodeSemiHeight)
    relLineTo (bottomWidth/2 - topWidth/2) height
    relLineTo (-bottomWidth) 0
    relLineTo ((bottomWidth - topWidth) /2) (-height)
    relLineTo topWidth 0
    -- closePath

    stroke

    doList (\ x -> do
       moveTo (posX-(bottomWidth/2)+x) (posY+nodeSemiHeight)
       relLineTo 0 nodeGateVertOffset) .
       List.map (* nodeGateSpacing) .
       (List.map fromIntegral) $ (seqInt nOutputs [])

    doList (\ x -> do
       moveTo (posX-(topWidth/2)+x) (posY-nodeSemiHeight)
       relLineTo 0 (-nodeGateVertOffset)) .
       List.map (* nodeGateSpacing) .
       (List.map fromIntegral) $ (seqInt nInputs [])

-- Draw an outer gate
drawGate :: Double -> Double -> Render ()
drawGate posX posY = do
    setSourceRGB 1 0 0
    arc posX posY gateRadius 0 (2*3.14159)
    fill

-- Add indexes to elements of a list
indexList :: Num b => [a] -> [(a,b)]
indexList = indexList_accu 0
   where
     indexList_accu curId [] = []
     indexList_accu curId (h:t) = (h,curId):(indexList_accu (curId+1) t)

-- Get the position (and whether it is a producer or not) of a gate given its name
-- and the list of gates of its node
findGateName :: Num a => String -> [OGate] -> Maybe (a,Bool)
findGateName = findGateName_accu (0,0)
  where
    findGateName_accu accu name [] = Nothing
    findGateName_accu (accu1,accu2) name ((OGate s b):t) =
      if s == name then Just (if b then accu1 else accu2, b)
      else if b then
        findGateName_accu (accu1+1,accu2) name t
      else
        findGateName_accu (accu1,accu2+1) name t

-- Get the position of a gate designated by a path
-- given a graph and a graph presentation
getGatePos :: OGraph -> OPath -> GraphPresentation -> (Double,Double)
getGatePos (OGraph gates nodes _) (OPath id name) presentation =
    if id == boundaryId then
       let Just (pos,producer) = findGateName name gates in
       (outerGateOffset + outerGateSpacing*pos,
       if producer then bottomOuterGates else topOuterGates)
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
          (posX-(topWidth/2)+nodeGateSpacing*(pos+1), posY+nodeSemiHeight)

-- Draw a graph given its presentation
drawGraph (OGraph gates nodes edges) presentation = do
    setSourceRGB 1 1 1
    paint
    drawGates topOuterGates (consumers gates)
    drawGates bottomOuterGates (producers gates)
    doList (\ (id,ONode s gates) ->
        let Just (posX,posY) = Map.lookup id presentation in
        drawNode posX posY (fromIntegral . length $ (producers gates))
                           (fromIntegral . length $ (consumers gates)))
           nodes
    where
     drawGates position =
      doList (\ ((OGate s _),id) ->
          drawGate (outerGateOffset + outerGateSpacing*id) position) .
      indexList

-- Find an element in a set satisfying a predicate
findSet predicate = Set.foldl (\ accu elem -> if (predicate elem) then Just elem else accu) Nothing

-- Draw the current selection
drawSelection _ NoSelection = return ()
drawSelection bounds (SelectNode id) =
    case (findSet (\ (i,_) -> i == id) bounds) of
     Nothing -> return ()
     Just (_,(BBox startx starty width height)) -> do
       setSourceRGB 0.5 0.5 0.5
       moveTo startx starty
       relLineTo width 0
       relLineTo 0 height
       relLineTo (-width) 0
       closePath
       stroke
drawSelection _ _ = return ()

-- Draw the whole scene (graph and selection if any)
drawScene drawState gsM = do
    gs <- liftIO (readMVar gsM)
    drawGraph (totalGraph gs) (presentation gs)
    drawSelection (nodeBB gs) (selection gs)

updateScene drawStateM gsM =    
    return True

-- Create a bounding box for a node given the number
-- of inputs, outputs and its position
makeNodeBoundingBox :: Int -> Int -> Double -> Double -> BoundingBox
makeNodeBoundingBox 0 0 x y =
    BBox (x-nodeSemiHeight) (y-nodeSemiHeight) (2*nodeSemiHeight) (2*nodeSemiHeight)

makeNodeBoundingBox nInputs nOutputs x y =
    let tot = max nInputs nOutputs in
    let width = (fromIntegral (tot+1))*nodeGateSpacing in
    BBox (x - width/2) (y - nodeSemiHeight) width (2*nodeSemiHeight)

-- Is this point in the bounding box ?
inBoundingBox :: Double -> Double -> BoundingBox -> Bool
inBoundingBox x y (BBox startx starty width height) =
    (x >= startx && y >= starty && x <= startx+width && y <= starty + height)

-- Handle a click based on the current state
handleClick drawStateM gsM drawWidget= do
    coords <- eventCoordinates
    st <- liftIO (readMVar drawStateM)
    if st == DNode then liftIO $ do
        gs <- liftIO (readMVar gsM)
        let (OGraph gates nodes edges) = totalGraph gs
        let pres = presentation gs
        let newId = maxOId nodes + 1
        let newGraph = OGraph gates ((newId,(ONode "" [])):nodes) edges
        let newPres = Map.insert newId coords pres
        let (x,y) = coords
        let newNodeBB = Set.insert (newId,makeNodeBoundingBox 0 0 x y) (nodeBB gs)
        modifyMVar_ gsM (\_ -> return (gs { totalGraph = newGraph,
                                            presentation = newPres,
                                            nodeBB = newNodeBB }))
        widgetQueueDraw drawWidget
    else if st == DSelect then liftIO $ do
        gs <- liftIO (readMVar gsM)
        let (x,y) = coords
        let searchResult = (Set.foldl (\ fnd (id,bb) -> if (inBoundingBox x y bb) then (Just id) else fnd) Nothing (nodeBB gs))
        newSelection <- return (case searchResult of
          Nothing -> NoSelection
          Just id -> SelectNode id)
        modifyMVar_ gsM (\_ -> return (gs { selection = newSelection }))
        widgetQueueDraw drawWidget
    else
        liftIO $ putStrLn ("Click handled at position " ++ (show coords))
    return True
