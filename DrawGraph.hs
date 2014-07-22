module DrawGraph where

-- Data structures
import Data.List as List
import Data.Set as Set
import Data.Foldable as Fold
import qualified Data.Map.Strict as Map
import OpenGraph as OG
import GraphPresentation
import TypeHierarchy
import SemanticScheme

-- GUI / Rendering stuff
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.Rendering.Cairo
import Control.Concurrent.MVar
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.Abstract.Widget


-- Current state of the pointer
data DrawingState =
     DSelect
   | DMoving OId
   | DNode
   | DEdge
   | DDrawing OPath Double Double
   | DDelete
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
       lastMouse :: Maybe (Double,Double),
       originalTypeSkeleton :: LambekSkel }

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

    doList (\ x -> do
       arc (posX-(bottomWidth/2)+x) (posY+nodeSemiHeight) gateRadius 0 (2*pi)
       fill) .
       List.map (* nodeGateSpacing) .
       (List.map fromIntegral) $ (seqInt nOutputs [])

    doList (\ x -> do
       arc (posX-(topWidth/2)+x) (posY-nodeSemiHeight) gateRadius 0 (2*pi)
       fill) .
       List.map (* nodeGateSpacing) .
       (List.map fromIntegral) $ (seqInt nInputs [])


-- Draw an outer gate
drawGate :: Double -> Double -> Render ()
drawGate posX posY = do
    setSourceRGB 1 0 0
    arc posX posY gateRadius 0 (2*pi)
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
    drawGates gates
    doList (\ (id,ONode s gates) ->
        let Just (posX,posY) = Map.lookup id presentation in
        drawNode posX posY (fromIntegral . length $ (consumers gates))
                           (fromIntegral . length $ (producers gates)))
           nodes
    doSet (drawEdge g presentation) edges
    where
     drawGates =
      doList (\ ((OGate s prod),id) ->
          let y = if prod then topOuterGates else bottomOuterGates in
          let x = outerGateOffset + outerGateSpacing*id in do
          setSourceRGB 0.8 0.8 0.8
          setLineWidth 1
          moveTo x 0
          lineTo x gateLineEnd
          stroke
          drawGate x y) .
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
    drawState <- liftIO (readMVar drawStateM)
    drawGraph (totalGraph gs) (presentation gs)
    drawTypeSkeleton (originalTypeSkeleton gs)
    drawSelection (nodeBB gs) (selection gs)
    for_ (lastMouse gs) $ \(x,y) ->
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

updateScene drawStateM (readGS,setGS) drawWidget = do
    (x,y) <- eventCoordinates
    gs <- liftIO $ readGS
    liftIO $ setGS $ gs { lastMouse = Just (x,y) }
    drawState <- liftIO $ readMVar drawStateM
    liftIO $ case drawState of
      DDrawing _ _ _ -> widgetQueueDraw drawWidget
      DNode -> widgetQueueDraw drawWidget
      DMoving id -> do 
       setGS $ gs { presentation = Map.insert id (x,y) $ presentation gs }
       widgetQueueDraw drawWidget
      _ -> return ()
    return True


-- Create a new graph state based on an input graph and a presentation
createGraphState g@(OGraph gates nodes edges) pres skel = 
    GraphState g pres NoSelection nodeBB gateBB Nothing skel
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

-- Same function, different type
createGraphStateFromEntry :: SkelEntry -> GraphState
createGraphStateFromEntry entry =
  createGraphState (skelGraph entry) (skelPres entry) (skeleton entry)

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
     case searchResult of
       Nothing -> createGateAt gs x y
       Just g -> (graph,Just g)     
     where
       graph = totalGraph gs
       searchResult = findBoundingBox x y (gateBB gs)
       createGateAt gs x y =
          (newGraph,matchingNodeGate)
          where
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

updateBoundingBoxes (readGS,setGS) = do
  gs <- readGS
  setGS . rebuildGraphState $ gs

rebuildGraphState gs =
  createGraphState (totalGraph gs)
                    (presentation gs)
                    (originalTypeSkeleton gs)

-- Handle a click based on the current state
handleClick drawStateM (readGS,setGS) drawWidget = do
    coords <- eventCoordinates
    let (x,y) = coords
    st <- liftIO (readMVar drawStateM)
    gs <- liftIO readGS
    let gotoState newState = modifyMVar_ drawStateM (\_ -> return newState)

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
        putStrLn ("Click handled at position " ++ (show coords))
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
        case searchResult of
          Nothing -> gotoState DEdge
          Just path -> do
            let (origX,origY) = getGatePos newGraph (presentation gs) path
            gotoState $ DDrawing path origX origY
      DDrawing gate _ _ -> do
        let (newGraph,maybeGate) = getOrCreateGate gs x y
        case maybeGate >>= (makeEdge newGraph gate) of
          Nothing ->
             gotoState DEdge
          Just edge -> do
             setGS (gs {
               totalGraph = newGraph {
                  edgesList=Set.insert edge (edgesList newGraph) } })
             gotoState DEdge
        widgetQueueDraw drawWidget
      DDelete -> do
        tryActions (x,y) [deleteNearestGate, deleteNearestEdge, deleteNearestNode]
        updateBoundingBoxes (readGS,setGS)
        widgetQueueDraw drawWidget
      _ -> return ()
    liftIO $ updateBoundingBoxes (readGS,setGS)
    return True
    where
      tryActions :: a -> [a -> IO Bool] -> IO ()
      tryActions pos [] = return ()
      tryActions pos (filter:others) = do
        successful <- filter pos
        if successful then
          return ()
        else
          tryActions pos others
      deleteNearestGate (x,y) = do
        gs <- readGS
        let searchResult = findBoundingBox x y (gateBB gs)
        case searchResult of
          Nothing -> return False
          Just gate -> do
            gs <- readGS
            setGS $ gs { totalGraph = deleteGate (totalGraph gs) gate }
            return True

      deleteNearestEdge (x,y) = do
        return False -- not implemented yet

      deleteNearestNode (x,y) = do
        gs <- liftIO readGS
        let searchResult = findBoundingBox x y (nodeBB gs)
        case searchResult of
          Nothing -> return False
          Just node -> do
             -- Delete all the gates in this node
             let oldGraph = totalGraph gs
             let newGraph = List.foldl deleteGate oldGraph . getGatesList oldGraph $ node
             -- Delete the node itself
             let newNodeList = List.filter (\ (id,_) -> id /= node) (nodesList newGraph)
             -- TODO: update bounding boxes
             gs <- readGS
             setGS $ gs { totalGraph = newGraph { nodesList = newNodeList }}
             return True
        where
          getGatesList :: OGraph -> OId -> [OPath]
          getGatesList graph nodeId =
            List.map (\ (OGate name _) -> OPath nodeId name) . getMaybeGates $ nodeId
            where
              getMaybeGates nodeId =
                 let lst = List.find (\ (i,_) -> i == nodeId) (nodesList graph) in
                 case lst of
                   Nothing -> []
                   Just (_,ONode _ gates) -> gates

      deleteGate :: OGraph -> OPath -> OGraph
      deleteGate oldGraph gate =
        -- Delete any edge involving this gate
        let edges = edgesList oldGraph in
        let newEdges = Set.filter (\ (OEdge p1 p2) -> p1 /= gate && p2 /= gate) edges in
        -- Delete the gate itself
        let (OPath nodeId gateId) = gate in
        let node = List.find (\ (id,_) -> id == nodeId) $ nodesList oldGraph in
        let newNode = node >>= (\ (_,ONode nodeName gatesList) -> return $ ONode nodeName $ List.filter (\ (OGate g _) -> g /= gateId) gatesList) in
        case newNode of
          Nothing -> oldGraph
          Just newNode ->
            let newNodesList = replaceItem nodeId newNode $ nodesList oldGraph in
            oldGraph { nodesList = newNodesList, edgesList = newEdges }
        where
          replaceItem :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
          replaceItem k v [] = []
          replaceItem k v ((k1,_):t)
            | (k1 == k) = (k,v):t
          replaceItem k v (h:t) = h:(replaceItem k v t)
          

handleRelease drawStateM (readGS,setGS) drawWidget = do
    st <- liftIO (readMVar drawStateM)
    let gotoState newState = modifyMVar_ drawStateM (\_ -> return newState)
    (x,y) <- eventCoordinates
    liftIO $ case st of
      DMoving _ -> do
        gs <- readGS
        setGS ((rebuildGraphState gs) { selection = (selection gs)})
        gotoState DSelect
        widgetQueueDraw drawWidget
      _ -> return ()
    return True

changeDrawingState gsM drawStateM cursors drawArea newState = do
   modifyMVar_ drawStateM (\oldState -> do
     case (oldState,newState) of
       (DSelect,DNode) -> resetSelection
       (DSelect,DEdge) -> resetSelection
       _ -> return ()
     window <- widgetGetParentWindow drawArea
     case newState of
       DEdge -> drawWindowSetCursor window (Just $ cursors !! 1)
       _ -> drawWindowSetCursor window Nothing
     return newState)
   where
     resetSelection = modifyMVar_ gsM (\gs -> return $ gs { selection = NoSelection })

drawHorizontallyCenteredText x y text left right = do
  (TextExtents xb _ w _ _ _) <- textExtents text
  moveTo (x - (w/2.0)) (y)
  textPath text
  fillPreserve
  setSourceRGB 0 0 0
  setLineWidth 0.7
  stroke
  (TextExtents xbl _ wl _ _ _) <- textExtents left
  moveTo (x - (w/2.0)-xbl-wl) y
  textPath left
  fillPreserve
  stroke
  (TextExtents xbr _ wr _ _ _) <- textExtents right
  moveTo (x + (w/2.0)+xbr) y
  textPath right
  fillPreserve
  stroke

-- Draw a type skeleton with the types above the gates
drawTypeSkeleton :: LambekSkel -> Render ()
drawTypeSkeleton skel = do
  _ <- drawSubSkeleton 1 NoParen "" "" skel
  return ()
  where
    drawHorizText x text l r =
      drawHorizontallyCenteredText x typeSkeletonPosition text l r
      
    drawSubSkeleton :: Double -> ParenthesisNeeded -> String -> String -> LambekSkel -> Render Double
    drawSubSkeleton offset _ l r t@(LSAtom s _) = do
      drawHorizText (offset*outerGateSpacing) (renderLS t) l r
      return (offset + 1)
    drawSubSkeleton offset _ l r t@(LSVar n) = do
      drawHorizText (offset*outerGateSpacing) (show n) l r
      return (offset + 1)

    drawSubSkeleton offset NoParen l r (LSLeft body arg) = do
      offset2 <- drawSubSkeleton offset AlwaysParen l "" arg
      drawHorizText ((offset2-0.5)*outerGateSpacing) "\\" "" ""
      drawSubSkeleton offset2 ParenRight "" r body
    drawSubSkeleton offset ParenRight l r skel@(LSLeft _ _) =
      drawSubSkeleton offset NoParen l r skel
    drawSubSkeleton offset _ l r skel@(LSLeft _ _) = do
      drawSubSkeleton offset NoParen (l++"(") (")"++r) skel

    drawSubSkeleton offset NoParen l r (LSRight body arg) = do
      offset2 <- drawSubSkeleton offset ParenLeft l "" body
      drawHorizText ((offset2-0.5)*outerGateSpacing) "/" "" ""
      drawSubSkeleton offset2 AlwaysParen "" r arg
    drawSubSkeleton offset ParenLeft l r skel@(LSRight _ _) =
      drawSubSkeleton offset NoParen l r skel
    drawSubSkeleton offset _ l r skel@(LSRight _ _) = do
      drawSubSkeleton offset NoParen (l++"(") (")"++r) skel

    
    