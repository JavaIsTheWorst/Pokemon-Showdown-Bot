{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Control.Concurrent.STM.TMChan (TMChan, writeTMChan, tryReadTMChan)
import           Control.Concurrent.MVar       (MVar, withMVar)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, runReaderT, ask, local)
import           GHC.Conc                      (atomically)
import           Data.Text                     as T
import           Data.Text.IO                  as T
import qualified Network.WebSockets            as WS
import qualified System.Console.ANSI           as ANSI
import qualified System.Console.ANSI.Types     as ANSI

data Env = Env {
  lock :: MVar (),
  chanLock :: MVar (),
  conn :: WS.Connection,
  chan :: TMChan T.Text}

readFromChan :: ANSI.Color -> ReaderT Env IO (Maybe T.Text)
readFromChan color = do
  env <- ask
  liftIO $ runReaderT (logText color "READING CHAN") env
  (Just maybeChanMessage) <- liftIO . atomically . tryReadTMChan $ chan env
  case maybeChanMessage of
    Nothing          -> do
      liftIO $ runReaderT (logText color "EMPTY CHAN") env
      return Nothing
    Just chanMessage -> do
      liftIO $ runReaderT (logText color $ "CHAN MESSAGE: " `T.append` chanMessage) env
      return $ Just chanMessage

logChan :: ANSI.Color -> T.Text -> ReaderT Env IO ()
logChan color str = do
  env <- ask
  liftIO $ runReaderT (logText color $ "WRITING TO CHAN: " `T.append` str) env
  liftIO $ atomically $ writeTMChan (chan env) str

basedOnChan :: ANSI.Color -> (Maybe T.Text -> ReaderT Env IO a) -> ReaderT Env IO a
basedOnChan color f = do
  env <- ask
  liftIO $ withMVar (chanLock env) (\_ ->
    runReaderT (readFromChan color >>= f) env)

basedOnChan_ :: ANSI.Color -> (Maybe T.Text -> ReaderT Env IO ()) -> ReaderT Env IO ()
basedOnChan_ color f = do
  env <- ask
  liftIO $ withMVar (chanLock env) (\_ ->
    runReaderT (readFromChan color >>= f) env)

logText :: ANSI.Color -> T.Text -> ReaderT Env IO ()
logText color str = do
  env <- ask
  liftIO $ withMVar (lock env) (\_ -> do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
    T.putStrLn str
    ANSI.setSGR [ANSI.Reset])

send :: T.Text -> ReaderT Env IO ()
send str = do
  env <- ask
  liftIO $ do
    T.putStrLn $ "SENDING TO SERVER: " `T.append` str  
    WS.sendTextData (conn env) str

sendWithColor :: ANSI.Color -> T.Text -> ReaderT Env IO ()
sendWithColor color str = do
  env <- ask
  liftIO $ withMVar (lock env) (\_ -> do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid color]
    runReaderT (send str) env
    ANSI.setSGR [ANSI.Reset])

useCommand :: ANSI.Color -> T.Text -> ReaderT Env IO ()
useCommand color message = say color "" message

say :: ANSI.Color -> T.Text -> T.Text -> ReaderT Env IO ()
say color room message = sendWithColor color $ room `T.append` "|" `T.append` message

pm :: ANSI.Color -> T.Text -> T.Text -> ReaderT Env IO ()
pm color username message = sendWithColor color $ "|/w " `T.append` username `T.append` "," `T.append` message

toId :: T.Text -> T.Text
toId = T.filter (\c -> c `elem` ['a'..'z'] || c `elem` ['1'..'9']) . T.toLower
