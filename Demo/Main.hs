module Main (main) where

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Reader

import Data.List
import qualified Data.ByteString.Char8 as B
import Data.IORef

import Text.URI

import AST.Position

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.SourceView

import Deadlock.Class
import Deadlock.Error
import Deadlock.DotRel

import System.Directory
import System.FilePath

main :: IO ()
main = do
  initGUI

  builder <- builderNew
  builderAddFromFile builder "Demo/demo.glade"

  window   <- builderGetObject builder castToWindow "window"
  mainarea <- builderGetObject builder castToScrolledWindow "mainarea"

  open     <- builderGetObject builder castToMenuItem "open"
  save     <- builderGetObject builder castToMenuItem "save"
  quit     <- builderGetObject builder castToMenuItem "quit"

  statusB  <- builderGetObject builder castToStatusbar "statusbar"
  cButton  <- builderGetObject builder castToButton "compButton" 
  errorBox <- builderGetObject builder castToHBox "errorBox"

  srcView  <- builderGetObject builder castToSourceView "sourceView"

  srcBuff <- makeSrcBuff
  setupSrcView srcView srcBuff
  listStore <- newErrorList

  stR <- mkState srcBuff statusB listStore

  treeDotStuff errorBox listStore

  open    `onActivateLeaf` openFile stR
  save    `onActivateLeaf` saveFile stR
  quit    `onActivateLeaf` mainQuit

  cButton `onClicked`      compAction stR
  window  `onDestroy`      mainQuit
          
  dragDestSet srcView [] [ActionCopy, ActionMove]
  dragDestAddURITargets srcView
  (srcView `on` dragDataReceived) (dropcall stR)

  widgetShowAll window
  mainGUI

dropcall stR ctx _ _ tstamp = do
  mURIs <- selectionDataGetURIs
  let mFile = do
        uriStr <- head `fmap` mURIs
        uri <- parseURI uriStr
        return (uriPath uri)
  lift $ openMFile stR mFile >> dragFinish ctx True True tstamp

newErrorList :: IO ErrorStore
newErrorList = listStoreNew []

treeDotStuff :: HBox -> ErrorStore -> IO ()
treeDotStuff errorBox listStore = do
  tv <- treeViewNewWithModel listStore

  posCol <- treeViewColumnNew
  treeViewColumnSetTitle posCol "Position"
  posText <- cellRendererTextNew
  cellLayoutPackStart posCol posText True
  cellLayoutSetAttributes posCol posText listStore 
                              (\ e -> [cellText := show (position e, posFeatureName e)])

  errorCol <- treeViewColumnNew
  treeViewColumnSetTitle errorCol "Error"
  errorText <- cellRendererTextNew
  cellLayoutPackStart errorCol errorText True
  cellLayoutSetAttributes errorCol errorText listStore 
                              (\ e -> [cellText := statusError e])

  treeViewAppendColumn tv posCol
  treeViewAppendColumn tv errorCol

  errorBox `containerAdd` tv

  tv `onRowActivated` expandedError listStore
  return ()


expandedError :: ErrorStore -> TreePath -> TreeViewColumn -> IO ()
expandedError es [index] _ = do
  e <- listStoreGetValue es index
  case contents e of
    CallOrdNotInContextOrd a b -> showOrders a b
    _ -> return ()

showOrders :: ProcOrder -> ProcOrder -> IO ()
showOrders a b = do
  w <- windowNew
  hbox <- hBoxNew True 2

  fileA <- generateImage a
  fileB <- generateImage b
  
  imgA <- imageNewFromFile fileA
  imgB <- imageNewFromFile fileB
  
  hbox `containerAdd` imgA
  hbox `containerAdd` imgB

  w `containerAdd` hbox    
  widgetShowAll w

makeSrcBuff :: IO SourceBuffer
makeSrcBuff = do
  Just eiffel <- eiffelM
  sourceBufferNewWithLanguage eiffel

setupSrcView :: SourceView -> SourceBuffer -> IO ()
setupSrcView src buff = do
  fn <- fontDescriptionNew
  fontDescriptionSetFamily fn "Monospace"

  textViewSetBuffer src buff
  widgetModifyFont src (Just fn)
  return ()

openFile :: StateR -> IO ()
openFile stR = do
  fc <- fileChooserDialogNew Nothing Nothing FileChooserActionOpen
        [("Open", ResponseOk)
        ,("Cancel", ResponseCancel)
        ]
  resp <- dialogRun fc
  openFileAction fc stR resp
  widgetDestroy fc

saveFile :: StateR -> IO ()
saveFile stR = do
  mFile <- path `fmap` readIORef stR
  case mFile of 
    Just _ -> saveState stR
    Nothing -> 
        do
          fc <- fileChooserDialogNew Nothing Nothing FileChooserActionSave
                [("Save", ResponseOk)
                ,("Cancel", ResponseCancel)
                ]
          resp <- dialogRun fc
          saveFileAction fc stR resp
          widgetDestroy fc

saveFileAction :: FileChooserDialog -> StateR -> ResponseId -> IO ()
saveFileAction fc stR ResponseOk = do
    mFile <- fileChooserGetFilename fc
    modifyIORef stR (\x -> x {path = mFile})
    saveState stR
saveFileAction _  _  ResponseCancel = return ()
saveFileAction _  _  _ = return ()

saveState :: StateR -> IO ()
saveState stR = do
  buff <- buffer `fmap` readIORef stR
  mFile <- path `fmap` readIORef stR
  maybe (return ()) (\ fp -> writeFile fp =<< bufferGetText buff) mFile

openFileAction :: FileChooserDialog -> StateR -> ResponseId -> IO ()
openFileAction fc stR ResponseOk = fileChooserGetFilename fc >>= openMFile stR
openFileAction _  _  ResponseCancel = return ()
openFileAction _  _  _ = return ()

openMFile :: StateR -> Maybe String -> IO ()
openMFile stR mFile = do
  modifyIORef stR (\x -> x {path = mFile})
  maybe (return ()) (setCurrentDirectory . fst . splitFileName) mFile
  maybe (return ()) (setBufferContent stR <=< readFile) mFile


eiffelM :: IO (Maybe SourceLanguage)
eiffelM = sourceLanguageManagerNew >>= 
         flip sourceLanguageManagerGetLanguage "eiffel"

setBufferContent :: StateR -> String -> IO ()
setBufferContent stR str = do
  st <- readIORef stR
  textBufferSetText (buffer st) str

bufferGetText :: SourceBuffer -> IO String
bufferGetText buff = get buff textBufferText 

compAction :: StateR -> IO ()
compAction stR = do
  buff <- stateBuffer stR
  str  <- bufferGetText buff

  stat <- stateStatusbar stR
  cId  <- statusbarGetContextId stat "Compile"

  errs <- stateErrors stR
  listStoreClear errs

  dr <- deadCheck (B.pack str)

  case dr of
    Just es -> do 
      statusbarPush stat cId "Errors in program."
      mapM_ (listStoreAppend errs) es
      return ()
    Nothing -> statusbarPush stat cId "Checked OK." >> return ()

statusError :: PosDeadError -> String
statusError = intercalate " :: " . lines . pretty . contents

type StateR = IORef State
type ErrorStore = ListStore PosDeadError
data State = State { buffer :: SourceBuffer
                   , status :: Statusbar
                   , errors :: ErrorStore
                   , path   :: Maybe FilePath
                   }

stateErrors :: StateR -> IO ErrorStore
stateErrors = return . errors <=< readIORef

stateBuffer :: StateR -> IO SourceBuffer
stateBuffer = return . buffer <=< readIORef

stateStatusbar :: StateR -> IO Statusbar
stateStatusbar = return . status <=< readIORef

mkState :: SourceBuffer -> Statusbar -> ErrorStore -> IO StateR
mkState buff statbar errs = newIORef (State buff statbar errs Nothing)
