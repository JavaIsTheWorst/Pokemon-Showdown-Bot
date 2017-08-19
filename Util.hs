{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Control.Concurrent.STM.TMChan (TMChan, writeTMChan, tryReadTMChan)
import           Control.Concurrent.MVar       (MVar, withMVar)
import           GHC.Conc                      (atomically)
import           Data.Text                  as T
import           Data.Text.IO               as T
import qualified Network.WebSockets         as WS
import qualified System.Console.ANSI        as ANSI
import qualified System.Console.ANSI.Types  as ANSI

readFromChan :: MVar () -> TMChan T.Text -> ANSI.Color -> IO (Maybe T.Text)
readFromChan lock chan color = do
  logText lock color "READING CHAN"
  (Just maybeChanMessage) <- atomically $ tryReadTMChan chan
  case maybeChanMessage of
    Nothing          -> do
      logText lock color "EMPTY CHAN"
      return Nothing
    Just chanMessage -> do
      logText lock color $ "CHAN MESSAGE: " `T.append` chanMessage
      return $ Just chanMessage

logChan :: MVar () -> TMChan T.Text -> ANSI.Color -> T.Text -> IO ()
logChan lock chan color str = do
  logText lock color $ "WRITING TO CHAN: " `T.append` str
  atomically $ writeTMChan chan str

basedOnChan :: MVar () -> MVar () -> TMChan T.Text -> ANSI.Color -> (Maybe T.Text -> IO a) -> IO a
basedOnChan lock chanLock chan color f = withMVar chanLock (\_ -> do
  maybeChanMessage <- readFromChan lock chan color
  f maybeChanMessage)

basedOnChan_ :: MVar () -> MVar () -> TMChan T.Text -> ANSI.Color -> (Maybe T.Text -> IO ()) -> IO ()
basedOnChan_ lock chanLock chan color f = withMVar chanLock (\_ -> do
  maybeChanMessage <- readFromChan lock chan color
  f maybeChanMessage)

logText :: MVar () -> ANSI.Color -> T.Text -> IO ()
logText lock color str = withMVar lock (\_ -> do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
  T.putStrLn str
  ANSI.setSGR [ANSI.Reset])

send :: MVar () -> WS.Connection -> T.Text -> IO ()
send lock conn str = do
  T.putStrLn $ "SENDING TO SERVER: " `T.append` str  
  WS.sendTextData conn str

sendWithColor :: MVar () -> WS.Connection -> ANSI.Color -> T.Text -> IO ()
sendWithColor lock conn color str = withMVar lock (\_ -> do
  ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
  send lock conn str
  ANSI.setSGR [ANSI.Reset])

useCommand :: MVar () -> WS.Connection -> ANSI.Color -> T.Text -> IO ()
useCommand lock conn color message = say lock conn color "" message

say :: MVar () -> WS.Connection -> ANSI.Color -> T.Text -> T.Text -> IO ()
say lock conn color room message = sendWithColor lock conn color $ room `T.append` "|" `T.append` message

pm :: MVar () -> WS.Connection -> ANSI.Color -> T.Text -> T.Text -> IO ()
pm lock conn color username message = sendWithColor lock conn color $ "|/w " `T.append` username `T.append` "," `T.append` message

toId :: T.Text -> T.Text
toId = T.filter (\c -> c `elem` ['a'..'z'] || c `elem` ['1'..'9']) . T.toLower
