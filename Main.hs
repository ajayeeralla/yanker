module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Data.Set as Set
import Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent.MVar

import DrawGraph
import OpenGraph


defaultGraphState :: GraphState
defaultGraphState =
    let initGraph = (OGraph [(OGate "a" True), (OGate "b" False), (OGate "c" False)]
                     [] (Set.empty)) in
    createGraphState initGraph Map.empty
    
main :: IO ()
main = do
    -- State of the interface
    graphState <- newMVar defaultGraphState
    drawState <- newMVar DSelect
    let changeDrawingState newState = modifyMVar_ drawState (\_ -> return newState)

    -- GUI setup
    initGUI
    builder <- builderNew
    builderAddFromFile builder "interface.glade"

    -- Get interesting widgets
    window <- builderGetObject builder castToWindow "window1"
    drawWidget <- builderGetObject builder castToDrawingArea "drawingarea1"
    selectButton <- builderGetObject builder castToToolButton "selectbutton"
    nodeButton <- builderGetObject builder castToToolButton "nodebutton"
    edgeButton <- builderGetObject builder castToToolButton "edgebutton"

    -- Connect signals to callbacks
    on window objectDestroy mainQuit
    widgetAddEvents drawWidget [PointerMotionMask, ButtonPressMask]
    on drawWidget draw (drawScene drawState graphState)
    on drawWidget motionNotifyEvent (updateScene drawState graphState drawWidget)
    on drawWidget buttonPressEvent (handleClick drawState graphState drawWidget)

    -- Buttons
    selectButton `onToolButtonClicked` (changeDrawingState DSelect)
    nodeButton `onToolButtonClicked` (changeDrawingState DNode)
    edgeButton `onToolButtonClicked` (changeDrawingState DEdge)
    
    widgetShowAll window
    mainGUI