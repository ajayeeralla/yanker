module DrawGraph where

import Data.List
import Graphics.Rendering.Cairo
import qualified Data.Map.Strict as Map
import OpenGraph as OG
import Control.Concurrent.MVar

type GraphPresentation = Map.Map OId (Double,Double)

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

doList f = foldl (\ accu elem -> accu >> (f elem)) (return ())

-- drawGate nInputs nOutputs :: Render ()
drawNode posX posY 0 0 = do
    setSourceRGB 0 0 0
    setLineWidth 1
    
    moveTo posX posY
    relMoveTo 0 nodeSemiHeight
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
    let height = (2*nodeSemiHeight)
    let topWidth = ((nInputs+1)*nodeGateSpacing)
    let bottomWidth = ((nOutputs+1)*nodeGateSpacing)

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
       map (* nodeGateSpacing) $
       (seqInt nOutputs [])

    doList (\ x -> do
       moveTo (posX-(topWidth/2)+x) (posY-nodeSemiHeight)
       relLineTo 0 (-nodeGateVertOffset)) .
       map (* nodeGateSpacing) $
       (seqInt nInputs [])


drawGate posX posY = do
    setSourceRGB 1 0 0
    arc posX posY gateRadius 0 (2*3.14159)
    fill

indexList_accu curId [] = []
indexList_accu curId (h:t) = (h,curId):(indexList_accu (curId+1) t)

indexList = indexList_accu 0

findGateName_accu accu name [] = Nothing
findGateName_accu (accu1,accu2) name ((OGate s b):t) =
    if s == name then Just (if b then accu1 else accu2, b)
    else if b then
      findGateName_accu (accu1+1,accu2) name t
    else
      findGateName_accu (accu1,accu2+1) name t

findGateName = findGateName_accu (0,0)

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

drawScene drawState graph = do
    g <- liftIO (readMVar graph)
    drawGraph g (Map.empty)

updateScene drawState graph =
    
    return True

handleClick drawState graph =
    return True
