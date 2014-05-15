module DrawGraph where

-- Data structures
import Data.List as List
import Data.Set as Set
import qualified Data.Map.Strict as Map
import OpenGraph as OG
import GraphPresentation

-- GUI / Rendering stuff
import Graphics.Rendering.Cairo
import Control.Concurrent.MVar
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Abstract.Widget


-- Current state of the pointer
data DrawingState = DSelect | DMoving OId | DNode | DEdge | DDrawing OPath Double Double
     deriving (Eq)

-- Current state of the object selection
data ElemSelection = SelectNode OId | SelectEdge OPath | NoSelection

-- Global state of the graph editor
data GraphState = GraphState {
       totalGraph :: OGraph,
       presentation :: GraphPresentation,
       selection :: ElemSelection,
       nodeBB :: (Map.Map OId BoundingBox),
       gateBB :: (Map.Map OPath BoundingBox),
       lastMouse :: (Double,Double) }


seqInt 0 accu = accu
seqInt n accu = seqInt (n-1) (n:accu)

doList f = List.foldl (\ accu elem -> accu >> (f elem)) (return ())
doSet f = Set.foldl (\ accu elem -> accu >> (f elem)) (return ())

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
    let topWidth = if nInputs > 0 then
                     ((fromIntegral nInputs+1)*nodeGateSpacing)
                   else
                     0
    let bottomWidth = if nOutputs > 0 then
                        ((fromIntegral nOutputs+1)*nodeGateSpacing)
                      else
                        0

    moveTo posX posY
    relMoveTo (topWidth/2) (-nodeSemiHeight)
    relLineTo (bottomWidth/2 - topWidth/2) height
    relLineTo (-bottomWidth) 0
    relLineTo ((bottomWidth - topWidth) /2) (-height)
    relLineTo topWidth 0
    -- closePath

    stroke

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


-- Draw an edge
drawEdge :: OGraph -> GraphPresentation -> OEdge -> Render ()
drawEdge graph pres (OEdge from to) = do
    let (fromX,fromY) = getGatePos graph pres from
    let (toX,toY) = getGatePos graph pres to
    setSourceRGB 0 0 0
    setLineWidth 1
    moveTo fromX fromY
    lineTo toX toY
    stroke

-- Draw a graph given its presentation
drawGraph g@(OGraph gates nodes edges) presentation = do
    setSourceRGB 1 1 1
    paint
    drawGates topOuterGates (producers gates)
    drawGates bottomOuterGates (consumers gates)
    doList (\ (id,ONode s gates) ->
        let Just (posX,posY) = Map.lookup id presentation in
        drawNode posX posY (fromIntegral . length $ (consumers gates))
                           (fromIntegral . length $ (producers gates)))
           nodes
    doSet (drawEdge g presentation) edges
    where
     drawGates position =
      doList (\ ((OGate s _),id) ->
          drawGate (outerGateOffset + outerGateSpacing*id) position) .
      indexList

-- Draw the current selection
drawSelection _ NoSelection = return ()
drawSelection bounds (SelectNode id) =
    case (Map.lookup id bounds) of
     Nothing -> return ()
     Just (BBox startx starty width height) -> do
       setSourceRGB 0.5 0.5 0.5
       moveTo startx starty
       relLineTo width 0
       relLineTo 0 height
       relLineTo (-width) 0
       closePath
       stroke
drawSelection _ _ = return ()

-- Draw the whole scene (graph and selection if any)
drawScene drawStateM gsM = do
    gs <- liftIO (readMVar gsM)
    let (x,y) = lastMouse gs
    drawState <- liftIO (readMVar drawStateM)
    drawGraph (totalGraph gs) (presentation gs)
    drawSelection (nodeBB gs) (selection gs)
    case drawState of
      DDrawing _ origX origY -> do
         setSourceRGB 0 0 0
         setLineWidth 1
         moveTo origX origY
         lineTo x y
         stroke
      DNode -> do
         drawNode x y 0 0
      _ -> return ()

updateScene drawStateM gsM drawWidget = do
    (x,y) <- eventCoordinates
    liftIO $ modifyMVar_ gsM (\gs -> return $ gs { lastMouse = (x,y) })
    drawState <- liftIO $ readMVar drawStateM
    liftIO $ case drawState of
      DDrawing _ _ _ -> widgetQueueDraw drawWidget
      DNode -> widgetQueueDraw drawWidget
      DMoving id -> do 
       modifyMVar_ gsM (\gs -> return $ gs {
          presentation = Map.insert id (x,y) $ presentation gs })
       widgetQueueDraw drawWidget
      _ -> return ()
    return True


-- Create a new graph state based on an input graph and a presentation
createGraphState g@(OGraph gates nodes edges) pres = 
    GraphState g pres NoSelection nodeBB gateBB (0,0)
    where
      nodeBB = List.foldl (\ m n -> Map.insert (fst n) (boundingBoxFromNode pres n) m)
                         Map.empty
                         nodes
      addGates boundMap nodeId gates = List.foldl (\ bm (OGate n _) ->
            Map.insert (OPath nodeId n) (boundingBoxFromGate g pres (OPath nodeId n)) bm)
                       boundMap
                       gates
      gateBB = List.foldl (\ m (id,ONode _ gates) -> addGates m id gates) outerGatesBB nodes
      outerGatesBB = addGates Map.empty boundaryId gates


-- Make an edge out of two gates, if there is one
-- (one has to be producer, the other consumer)
-- and no edge is currently bound to the consumer
makeEdge graph path1 path2 =
    case (getGate path1 graph, getGate path2 graph) of
     (Just True, Just False) ->
         if checkAddEdge graph (OEdge path1 path2) then
           Just $ OEdge path1 path2
         else
           Nothing 
     (Just False, Just True) ->
         if checkAddEdge graph (OEdge path2 path1) then
           Just $ OEdge path2 path1
         else
           Nothing
     _ -> Nothing

ifNothing Nothing b = b
ifNothing a _ = a

addMaybeGate (Just (OPath oid gateName)) (Just producer) g@(OGraph _ nodes _)  =
    Just (g { nodesList = List.map changeNodes nodes })
    where
      changeNodes n@(id,ONode name gates) =
         if id /= oid then
            n
         else
            (id,ONode name ((OGate gateName producer):gates))
addMaybeGate _ _ _ = Nothing

getOrCreateGate gs x y =
     (newGraph, searchResult `ifNothing` matchingNodeGate)
     where
       searchResult = findBoundingBox x y (gateBB gs)
       graph = totalGraph gs
       matchingNode = findBoundingBox x y (nodeBB gs)
       isClickProducer oid =
         (Map.lookup oid (nodeBB gs) >>= return . inLowerPartOfBBox x y) == Just True
       matchingNodeGate = (matchingNode >>= (\ oid ->
           let Just fresh = freshGateName graph oid in -- this is safe
           return $ OPath oid fresh))
       maybeProducer = matchingNode >>= return . isClickProducer
       maybeGraph = addMaybeGate matchingNodeGate maybeProducer graph
       newGraph = (case maybeGraph of
                       Just g -> g
                       Nothing -> graph)


-- Handle a click based on the current state
handleClick drawStateM gsM drawWidget = do
    coords <- eventCoordinates
    let (x,y) = coords
    st <- liftIO (readMVar drawStateM)
    gs <- liftIO (readMVar gsM)
    let gotoState newState = modifyMVar_ drawStateM (\_ -> return newState)
    let setGS newGS = modifyMVar_ gsM (\_ -> return newGS)

        
    liftIO $ case st of
      DNode -> do
        let (OGraph gates nodes edges) = totalGraph gs
        let pres = presentation gs
        let newId = maxOId nodes + 1
        let newGraph = OGraph gates ((newId,(ONode "" [])):nodes) edges
        let newPres = Map.insert newId coords pres
        let newNodeBB = Map.insert newId (makeNodeBoundingBox 0 0 x y) (nodeBB gs)
        setGS (gs { totalGraph = newGraph,
                    presentation = newPres,
                    nodeBB = newNodeBB })
        widgetQueueDraw drawWidget
      DSelect -> do
        let searchResult = findBoundingBox x y (nodeBB gs)
        newSelection <- case searchResult of
          Nothing -> return NoSelection
          Just id -> do
            gotoState (DMoving id)
            return $ SelectNode id
        setGS (gs { selection = newSelection })
        widgetQueueDraw drawWidget
      DEdge -> do
        let (newGraph,searchResult) = getOrCreateGate gs x y
        setGS (gs { totalGraph = newGraph })
        gotoState $ case searchResult of
          Nothing -> DEdge
          Just path -> DDrawing path x y
      DDrawing gate _ _ -> do
        let (newGraph,maybeGate) = getOrCreateGate gs x y
        case maybeGate >>= (makeEdge newGraph gate) of
          Nothing ->
             gotoState DEdge
          Just edge -> do
             putStrLn ("Current edges list is "++(show $ edgesList newGraph))
             setGS (gs {
               totalGraph = newGraph {
                  edgesList=Set.insert edge (edgesList newGraph) } })
             gotoState DEdge
        widgetQueueDraw drawWidget
      _ -> putStrLn ("Click handled at position " ++ (show coords))
    return True


handleRelease drawStateM gsM drawWidget = do
    st <- liftIO (readMVar drawStateM)
    let gotoState newState = modifyMVar_ drawStateM (\_ -> return newState)
    (x,y) <- eventCoordinates
    liftIO $ case st of
      DMoving _ -> do
        modifyMVar_ gsM (\gs -> return $
          ((createGraphState (totalGraph gs) (presentation gs)) { selection = (selection gs)}))
        gotoState DSelect
        widgetQueueDraw drawWidget
      _ -> return ()
    return True

changeDrawingState gsM drawStateM newState = do
   modifyMVar_ drawStateM (\oldState -> do 
     case (oldState,newState) of
       (DSelect,DNode) -> resetSelection
       (DSelect,DEdge) -> resetSelection
       _ -> return ()
     return newState)
   where
     resetSelection = modifyMVar_ gsM (\gs -> return $ gs { selection = NoSelection })