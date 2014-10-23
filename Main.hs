module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Gdk.EventM
import Graphics.UI.Gtk.ModelView as Model
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State
import Text.ParserCombinators.Parsec
import Data.Set as Set
import Data.Map as Map
import Data.Foldable as Fold
import Control.Concurrent.MVar
import Control.Concurrent.MVar

import DrawGraph
import OpenGraph
import TypeHierarchy
import TypeDatabase
import SemanticScheme
import Paths_yanker
import Data.List as List

-- The type of an entry in the type list,
-- storing its number of occurrences, visibility
-- and the index of the first skeleton matching it
data TypeEntry = TypeEntry {
    nbOccurrences :: Int
  , lambekType :: LambekFun
  , currentlyVisible :: Bool
  , firstSkelIndex :: Int }


defaultGraphState :: GraphState
defaultGraphState =
    let initSkel = LSVar 1 in
    createGraphState (emptyGraphFromSkel initSkel) Map.empty initSkel


loadTypeDatabase :: IO [TypeEntry]
loadTypeDatabase = do
    typePath <- getDataFileName "data/short.types.db"
    typeFile <- readFile typePath
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
        (a,b) <- parserLineDB
        eof
        return $ TypeEntry a b True 0 -- True is the default visibility

createAddDialog builder skelStore typeStore = do -- skelUniqueId = do
    dialogPath <- getDataFileName "gui/add-skeleton-dialog.glade"
    builderAddFromFile builder dialogPath
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
            appendToStore (defaultSkelEntry lambekSkel)
            widgetHide dialog
      appendToStore lambekSkel = do
        listStorePrepend skelStore lambekSkel
        updateSkelIndices typeStore skelStore

createFileDialog :: FileChooserAction -> Window -> (String -> IO ()) -> IO ()
createFileDialog action parent callBack = do
  let (title,name) = case action of
        FileChooserActionOpen -> (Just "Open a scheme","Open")
        FileChooserActionSave -> (Just "Save the scheme","Save")
        _ -> (Nothing,"Accept")
  dialog <- fileChooserDialogNew
            title
            (Just parent)
            action
            [(name,ResponseAccept),("Cancel",ResponseCancel)]
  fileChooserSetSelectMultiple dialog False
  widgetShow dialog
  response <- dialogRun dialog
  case response of
    ResponseAccept -> do
      Just fname <- fileChooserGetFilename dialog
      callBack fname
    _ -> return ()
  widgetHide dialog

reportError :: Window -> String -> IO ()
reportError mainWindow msg = do
  dialog <- messageDialogNew (Just mainWindow) [] MessageError ButtonsOk msg
  dialogRun dialog
  widgetHide dialog
  return ()

tryLoadSemScheme :: Window -> ListStore SkelEntry -> String -> IO ()
tryLoadSemScheme mainWindow skelStore fname = do
  newScheme <- loadSemanticScheme fname
  case newScheme of
    Right scheme -> do
      listStoreClear skelStore
      Fold.for_ scheme (listStoreAppend skelStore)
    Left error -> reportError mainWindow error

trySaveSemScheme :: Window -> ListStore SkelEntry -> String -> IO ()
trySaveSemScheme mainWindow skelStore fname = do
  currentScheme <- listStoreToList skelStore
  error <- saveSemanticScheme fname currentScheme
  case error of
    Just error -> reportError mainWindow error
    Nothing -> return ()
      

createAboutDialog builder = do
    dialogPath <- getDataFileName "gui/about-dialog.glade"
    builderAddFromFile builder dialogPath
    aboutDialog <- builderGetObject builder castToDialog "aboutdialog"
    widgetShowAll aboutDialog

-- This function actually does 2 things:
     -- Update the list of types matching the selected skeleton
     -- Update the graphical editor with the corresponding graph
updateTypesAndEditor modelTypes modelSkel viewSkel (readGS,setGS) drawWidget = do
    selection <- treeViewGetSelection viewSkel
    selected <- treeSelectionGetSelected selection
    Fold.forM_ selected $ \treeIter -> do
      -- Update the current types
      let idx = listStoreIterToIndex treeIter
      skelEntry <- listStoreGetValue modelSkel idx
      nbTypes <- listStoreGetSize modelTypes
      forNTimes nbTypes $ \i -> do
        entry <- listStoreGetValue modelTypes i
        let newVis = firstSkelIndex entry == idx
        listStoreSetValue modelTypes i $ entry { currentlyVisible = newVis }
        
      -- Update the graphical editor
      let newGs = createGraphStateFromEntry skelEntry
      setGS newGs
      widgetQueueDraw drawWidget

forNTimes n =
   Fold.for_ [0..(n-1)]

updateSkelIndices :: ListStore TypeEntry -> ListStore SkelEntry -> IO ()
updateSkelIndices modelTypes modelSkel = do
    nbSkels <- listStoreGetSize modelSkel
    nbTypes <- listStoreGetSize modelTypes
    -- for each type
    forNTimes nbTypes $ \i -> do
      entry <- listStoreGetValue modelTypes i
      -- reset its current skeleton
      listStoreSetValue modelTypes i (entry { firstSkelIndex = -1 })
    
    -- for each skeleton
    forNTimes nbSkels $ \j -> do
      skelEntry <- listStoreGetValue modelSkel j
      let skel = skeleton skelEntry
      -- for each type
      forNTimes nbTypes $ \i -> do
        entry <- listStoreGetValue modelTypes i
        if firstSkelIndex entry == -1 && lambekType entry `isMatchedBy` skel then
            listStoreSetValue modelTypes i (entry { firstSkelIndex = j })
        else
            return ()

deleteCurrentSkel skelStore skelView = do
   selection <- treeViewGetSelection skelView
   selected <- treeSelectionGetSelected selection
   Fold.forM_ selected $ \treeIter -> do
     listStoreRemove skelStore $ listStoreIterToIndex treeIter

visCol :: ColumnId TypeEntry Bool
visCol = makeColumnIdBool 0

-- Update the graph state and update the corresponding entry in the skeleton list
setGraphState gsM skelStore skelView newGs = do
  modifyMVar_ gsM (\_ -> return newGs)
  selection <- treeViewGetSelection skelView
  selected <- treeSelectionGetSelected selection
  Fold.forM_ selected $ \treeIter -> do
    let idx = listStoreIterToIndex treeIter
    listStoreSetValue skelStore idx
      (SkelEntry (originalTypeSkeleton newGs) (totalGraph newGs) (presentation newGs))

main :: IO ()
main = do
    -- State of the interface
    graphState <- newMVar defaultGraphState -- will be overridden later in updateTypesAndEditor
    drawState <- newMVar DSelect
    
    -- Create stores and lists
    skelStore <- listStoreNew defaultSemanticScheme
    typeList <- loadTypeDatabase
    typeStore <- listStoreNew typeList

    -- GUI setup
    initGUI
    builder <- builderNew
    interfacePath <- getDataFileName "gui/interface.glade"
    builderAddFromFile builder interfacePath

    -- Cursors for the drawing area
    crossCursor <- cursorNew Crosshair
    pencilCursor <- cursorNew Pencil
    let cursors = [crossCursor,pencilCursor]

    -- Get interesting widgets (main window)
    window <- builderGetObject builder castToWindow "window1"
    drawWidget <- builderGetObject builder castToDrawingArea "drawingarea1"
    
    [selectButton, nodeButton, edgeButton, delElemButton] <-
      sequence . (List.map (builderGetObject builder castToToolButton)) $
      ["selectbutton", "nodebutton", "edgebutton", "deleteelement"]
      
    treeViewTypes <- builderGetObject builder castToTreeView "treeview2"
    treeViewSkels <- builderGetObject builder castToTreeView "treeview1"
    addSkelButton <- builderGetObject builder castToButton "buttonaddrule"
    delSkelButton <- builderGetObject builder castToButton "buttondelrule"
    
    [openButton, saveButton, saveAsButton, quitButton, aboutButton] <-
      sequence (List.map
                (\x -> builderGetObject builder castToImageMenuItem ("imagemenuitem-"++x))
      ["open","save","save-as","quit","about"])

    -- Graph state manipulation helpers
    let readGS = readMVar graphState
    let setGS = setGraphState graphState skelStore treeViewSkels
    let gs = (readGS,setGS)

    -- Draw state manipulation helper
    let changeState = changeDrawingState graphState drawState cursors drawWidget

    -- Connect signals to callbacks (main window)
    on window objectDestroy mainQuit
    widgetAddEvents drawWidget [PointerMotionMask, ButtonPressMask]
    on drawWidget draw (drawScene drawState graphState)
    on drawWidget motionNotifyEvent (updateScene drawState gs drawWidget)
    on drawWidget buttonPressEvent (handleClick drawState gs drawWidget)
    on drawWidget buttonReleaseEvent (handleRelease drawState gs drawWidget)
    -- on drawWidget leaveNotify (\_ -> return ())
    on treeViewSkels cursorChanged (updateTypesAndEditor typeStore skelStore treeViewSkels gs drawWidget)
    on skelStore rowsReordered (\ _ _ _ -> updateSkelIndices typeStore skelStore)
    on skelStore rowInserted (\ _ _ -> updateSkelIndices typeStore skelStore)
    on skelStore rowDeleted (\ _ -> updateSkelIndices typeStore skelStore)
      
    -- Buttons
    selectButton `onToolButtonClicked` (changeState DSelect)
    nodeButton `onToolButtonClicked` (changeState DNode)
    edgeButton `onToolButtonClicked` (changeState DEdge)
    delElemButton `onToolButtonClicked` (changeState DDelete)
    on addSkelButton buttonActivated (createAddDialog builder skelStore typeStore)
    on delSkelButton buttonActivated (deleteCurrentSkel skelStore treeViewSkels)

    -- Menu items
    on openButton menuItemActivated
      (createFileDialog FileChooserActionOpen window (tryLoadSemScheme window skelStore))
    on saveButton menuItemActivated
      (createFileDialog FileChooserActionSave window (trySaveSemScheme window skelStore))
    on quitButton menuItemActivated (exitApplication window)
    on aboutButton menuItemActivated (createAboutDialog builder)

    -- Load type database
    colOccur <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle colOccur "Num"
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart colOccur renderer False
    Model.cellLayoutSetAttributes colOccur renderer typeStore
           $ (\entry -> [Model.cellText := show . nbOccurrences $ entry])
    Model.treeViewAppendColumn treeViewTypes colOccur
    
    col <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle col "Type"
    renderer <- Model.cellRendererTextNew
    Model.cellLayoutPackStart col renderer False
    Model.cellLayoutSetAttributes col renderer typeStore
           $ (\entry -> [Model.cellText := renderLF . lambekType $ entry])
    Model.treeViewAppendColumn treeViewTypes col

    fModel <- treeModelFilterNew typeStore []
    customStoreSetColumn typeStore visCol currentlyVisible
    treeModelFilterSetVisibleColumn fModel visCol
    
    treeViewSetModel treeViewTypes fModel

    -- Add skeleton list
    treeViewSetModel treeViewSkels skelStore

    colSkel <- Model.treeViewColumnNew
    Model.treeViewColumnSetTitle colSkel "Pattern"
    Model.cellLayoutPackStart colSkel renderer False
    Model.cellLayoutSetAttributes colSkel renderer skelStore
           $ (\tp -> [Model.cellText := renderLS . skeleton $ tp])
    Model.treeViewAppendColumn treeViewSkels colSkel

    -- Update the graph and matching types for the first time
    selection <- treeViewGetSelection treeViewSkels
    treeSelectionSelectPath selection [0]
    updateTypesAndEditor typeStore skelStore treeViewSkels gs drawWidget
    
    widgetShowAll window
    mainGUI

    where
      exitApplication window = do
        widgetHide window
        mainQuit
