module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Data.Set as Set
import Data.Map as Map
import Control.Concurrent.MVar
import Control.Concurrent.MVar

import DrawGraph
import OpenGraph
import TypeHierarchy
import TypeDatabase
import Data.List as List

defaultGraphState :: GraphState
defaultGraphState =
    let initGraph = (OGraph [(OGate "a" False), (OGate "b" True), (OGate "c" True)]
                     [] (Set.empty)) in
    createGraphState initGraph Map.empty

loadTypeDatabase :: IO [(Int,LambekFun)]
loadTypeDatabase = do
    typeFile <- readFile "data/short.types.db"
    typeLines <- return $ lines typeFile
    let typeDB = sequence . List.map (parse parserWithEof "") $ typeLines
    case typeDB of
       Left error -> do
         print error
         return []
       Right db -> do
         -- putStrLn (show db)
         return db
    where
      parserWithEof = do
        res <- parserLineDB
        eof
        return res

createAddDialog builder skelStore = do
    builderAddFromFile builder "gui/add-skeleton-dialog.glade"
    addSkeletonDialog <- builderGetObject builder castToDialog "dialog1"
    cancelButton <- builderGetObject builder castToButton "buttoncancel"
    addButton <- builderGetObject builder castToButton "buttonadd"
    lineEntry <- builderGetObject builder castToEntry "entry1"
    -- Connect signals to callbacks (add dialog)
    on addSkeletonDialog objectDestroy (widgetHide addSkeletonDialog)
    on cancelButton buttonActivated (widgetHide addSkeletonDialog)
    on addButton buttonActivated (tryAdd addSkeletonDialog lineEntry)
    widgetShowAll addSkeletonDialog
    where
      tryAdd dialog lineEntry = do
        contents <- entryGetText lineEntry
        let parseResult = parse parserLS "(unknown)" contents
        case parseResult of
          Left error -> print error
          Right lambekSkel -> do
            appendToStore lambekSkel
            putStrLn (show lambekSkel)
            widgetHide dialog
      appendToStore lambekSkel =
         listStoreAppend skelStore lambekSkel

main :: IO ()
main = do
    -- State of the interface
    graphState <- newMVar defaultGraphState
    drawState <- newMVar DSelect
    let changeState = changeDrawingState graphState drawState
        
    -- Create stores and lists
    skelStore <- listStoreNew []
    typeList <- loadTypeDatabase
    typeStore <- listStoreNew typeList

    -- GUI setup
    initGUI
    builder <- builderNew
    builderAddFromFile builder "gui/interface.glade"

    -- Get interesting widgets (main window)
    window <- builderGetObject builder castToWindow "window1"
    drawWidget <- builderGetObject builder castToDrawingArea "drawingarea1"
    selectButton <- builderGetObject builder castToToolButton "selectbutton"
    nodeButton <- builderGetObject builder castToToolButton "nodebutton"
    edgeButton <- builderGetObject builder castToToolButton "edgebutton"
    treeViewTypes <- builderGetObject builder castToTreeView "treeview2"
    treeViewSkels <- builderGetObject builder castToTreeView "treeview1"
    addSkelButton <- builderGetObject builder castToButton "buttonaddrule"
    upSkelButton <- builderGetObject builder castToButton "buttonmoveup"
    downSkelButton <- builderGetObject builder castToButton "buttonmovedown"

    -- Connect signals to callbacks (main window)
    on window objectDestroy mainQuit
    widgetAddEvents drawWidget [PointerMotionMask, ButtonPressMask]
    on drawWidget draw (drawScene drawState graphState)
    on drawWidget motionNotifyEvent (updateScene drawState graphState drawWidget)
    on drawWidget buttonPressEvent (handleClick drawState graphState drawWidget)
    on drawWidget buttonReleaseEvent (handleRelease drawState graphState drawWidget)
    -- on skelStore rowsReordered (rebuildFilters
    -- rowInserted (TreePath -> TreeIter -> IO ())
    -- rowDeleted (TreePath -> TreeIter -> IO ())

    -- Buttons
    selectButton `onToolButtonClicked` (changeState DSelect)
    nodeButton `onToolButtonClicked` (changeState DNode)
    edgeButton `onToolButtonClicked` (changeState DEdge)
    on addSkelButton buttonActivated (createAddDialog builder skelStore)

    -- Load type database
    treeViewSetModel treeViewTypes typeStore

    colOccur <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle colOccur "Num"
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart colOccur renderer False
    Model.cellLayoutSetAttributes colOccur renderer typeStore
           $ (\(occur,tp) -> [Model.cellText := show occur])
    Model.treeViewAppendColumn treeViewTypes colOccur
    
    col <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle col "Type"
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart col renderer False
    Model.cellLayoutSetAttributes col renderer typeStore
           $ (\(occur,tp) -> [Model.cellText := renderLF tp])
    Model.treeViewAppendColumn treeViewTypes col

    -- Add skeleton list
    treeViewSetModel treeViewSkels skelStore

    colSkel <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle colSkel "Pattern"
    Model.cellLayoutPackStart colSkel renderer False
    Model.cellLayoutSetAttributes colSkel renderer skelStore
           $ (\tp -> [Model.cellText := renderLS tp])
    Model.treeViewAppendColumn treeViewSkels colSkel
    
    widgetShowAll window
    mainGUI
