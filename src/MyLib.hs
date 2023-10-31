{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module MyLib where

import Brick
import Brick qualified as M
import Brick.AttrMap qualified as A
import Brick.BChan qualified as BChan
import Brick.Types qualified as T
import Brick.Widgets.Border
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Border.Style
import Brick.Widgets.Center
import Brick.Widgets.Center qualified as C
import Brick.Widgets.List
import Brick.Widgets.List qualified as L
import Control.Applicative (many, some)
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Except
import Dap
import Dap.Protocol
import Dap.Session
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Attoparsec.ByteString qualified as P
import Data.Attoparsec.ByteString.Char8 qualified as PC
import Data.ByteString qualified as BS
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Char (digitToInt, isDigit)
import Data.Maybe (fromMaybe)
import Data.Scientific (base10Exponent)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as TIO
import Data.Vector qualified as Vec
import Data.Vector qualified as Vector
import Data.Word (Word8)
import Graphics.Vty qualified
import Graphics.Vty qualified as V
import Lens.Micro
import Lens.Micro.Aeson
import Lens.Micro.Mtl (use)
import Lens.Micro.TH
import Network.Simple.TCP
import System.FilePath (takeDirectory, takeFileName)
import Text.Pretty.Simple (pPrint)
import Text.Read (readMaybe)
import UnliftIO.Async
import UnliftIO.STM

type FileContentList = L.List () Text

data DapStatus = Idle | Initialized | Stopped

data St = St
  { _dapEnv :: DapEnv
  , _fileContent :: FileContentList
  , _breakpointLines :: [Int]
  , _dapStatus :: DapStatus
  , _activeSession :: Maybe Session
  , _logs :: [String]
  }
makeLenses ''St

ui :: St -> [Widget ()]
ui st = [app]
 where
  app =
    joinBorders $
      withBorderStyle unicode $
        border $
          left <+> vBorder <+> right
  left =
    vBox [contents]

  right =
    case st ^. dapStatus of
      Idle -> txt "Idle"
      Initialized -> txt "Initialized"
      Stopped -> txt "Stopped"

  contents = L.renderListWithIndex (listDrawElement (st ^. breakpointLines)) True (st ^. fileContent)

listDrawElement :: [Int] -> Int -> Bool -> Text -> Widget ()
listDrawElement bps idx selected line
  | selected && idx `elem` bps = txt $ "B " <> line <> "<selected>"
  | selected = txt $ "  " <> line <> "<selected>"
  | idx `elem` bps = txt $ "B " <> line
  | otherwise = txt $ "  " <> line

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    []

appEvent :: T.BrickEvent () Event -> T.EventM () St ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'k') [] -> modify (fileContent %~ listMoveUp)
    V.EvKey (V.KChar 'j') [] -> modify (fileContent %~ listMoveDown)
    V.EvKey (V.KChar 'i') [] -> do
      modify (logs <>~ ["Init"])
      env <- use dapEnv
      _ <- initialize env
      pure ()
    V.EvKey (V.KChar 'b') [] -> do
      f <- use fileContent
      case f ^. listSelectedL of
        Just i -> modify (breakpointLines %~ (i :))
        Nothing -> pure ()
    V.EvKey V.KEsc [] -> M.halt
    _ -> pure ()
appEvent (T.AppEvent e) = case e of
  EventInitialized _session -> do
    modify (logs <>~ ["Initialize"])
    modify (dapStatus .~ Initialized)
    pure ()
  EventStopped _session -> do
    modify (logs <>~ ["Stopped"])
    modify (dapStatus .~ Stopped)
appEvent _ = return ()

theApp :: M.App St Event ()
theApp =
  M.App
    { M.appDraw = ui
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

initState :: DapEnv -> FileContentList -> St
initState env fl =
  St
    { _dapEnv = env
    , _fileContent = fl
    , _breakpointLines = []
    , _dapStatus = Idle
    , _activeSession = Nothing
    , _logs = []
    }

data Event
  = EventStopped Session
  | EventInitialized Session

main :: IO ()
main = do
  env <- newEnv
  let filePath = "/home/mahdi/tmp/app.js"
  fileContents <- TIO.readFile filePath
  let fl = L.list () (Vector.fromList $ T.lines fileContents) 1
  eventChan <- BChan.newBChan 10
  addAfterEventCallback env DapEventStopped $ \session ->
    BChan.writeBChan eventChan (EventStopped session)
  addAfterEventCallback env DapEventInitialized $ \session ->
    BChan.writeBChan eventChan (EventInitialized session)

  let buildVty = Graphics.Vty.mkVty Graphics.Vty.defaultConfig
  initialVty <- buildVty
  void $ M.customMain initialVty buildVty (Just eventChan) theApp (initState env fl)
  -- shutdown env

