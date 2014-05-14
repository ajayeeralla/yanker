module Main where

import Graphics.UI.Gtk
-- import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.EventM

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Data.Set as Set
import Control.Concurrent.MVar
import Control.Concurrent.MVar

import DrawGraph
import OpenGraph
-- http://projects.haskell.org/gtk2hs/docs/tutorial/glade/

data DrawingState = DSelect | DMoving OId | DNode | DEdge | DDrawing OPath

main = do
    initGUI
    graph <- newMVar (OGraph [(OGate "a" True), (OGate "b" False), (OGate "c" False)]
                     [] (Set.empty))
    drawState <- newMVar DSelect
    builder <- builderNew
    builderAddFromFile builder "interface.glade"
    window <- builderGetObject builder castToWindow "window1"
    drawWidget <- builderGetObject builder castToDrawingArea "drawingarea1"

    on window objectDestroy mainQuit
    widgetAddEvents drawWidget [PointerMotionMask, ButtonPressMask]
    on drawWidget draw (drawScene drawState graph)
    on drawWidget motionNotifyEvent (updateScene drawState graph)
    on drawWidget buttonPressEvent (handleClick drawState graph)

    let changeDrawingState newState = modifyMVar_ drawState (\_ -> return newState)

    selectButton <- builderGetObject builder castToToolButton "selectbutton"
    nodeButton <- builderGetObject builder castToToolButton "nodebutton"
    edgeButton <- builderGetObject builder castToToolButton "edgebutton"
    selectButton `onToolButtonClicked` (changeDrawingState DSelect)
    nodeButton `onToolButtonClicked` (changeDrawingState DNode)
    edgeButton `onToolButtonClicked` (changeDrawingState DEdge)
    
    widgetShowAll window
    mainGUI