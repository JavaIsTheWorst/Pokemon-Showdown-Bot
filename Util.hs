{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, runReaderT, ask)
import           Data.IORef                    (IORef, readIORef, writeIORef)
import qualified Control.Concurrent.Lock       as Lock
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS
import qualified System.Console.ANSI           as ANSI

import qualified Config
import qualified Plugins.Chess.Types           as Chess
import qualified Plugins.GreatestIdea.Types    as Gesti
import qualified Plugins.Mafia.Types           as Mafia
import qualified Plugins.Uno.Types             as Uno

data Env = Env {
  lock :: Lock.Lock,
  conn :: WS.Connection,
  unoGames :: IORef (Map.Map T.Text Uno.UNOGame),
  chessGame :: IORef (Maybe Chess.ChessGame),
  gestiGame :: IORef (Maybe Gesti.GestiGame),
  mafiaGames :: IORef (Map.Map T.Text Mafia.MafiaGame)}

readFromChan :: (ANSI.ColorIntensity, ANSI.Color) -> IORef T.Text -> ReaderT Env IO T.Text
readFromChan color chan = do
  logText color "READING CHAN"
  chanMessage <- liftIO $ readIORef chan
  logText color $ "CHAN MESSAGE: " `T.append` chanMessage
  return chanMessage

logChan :: (ANSI.ColorIntensity, ANSI.Color) -> IORef T.Text -> T.Text -> ReaderT Env IO ()
logChan color chan str = do
  logText color $ "WRITING TO CHAN: " `T.append` str
  liftIO $ writeIORef chan str

basedOnChan :: (ANSI.ColorIntensity, ANSI.Color) -> IORef T.Text -> Lock.Lock -> (T.Text -> ReaderT Env IO a) -> ReaderT Env IO a
basedOnChan color chan chanLock f =
  ask >>= liftIO . Lock.with chanLock . runReaderT (readFromChan color chan >>= f)

basedOnChan_ :: (ANSI.ColorIntensity, ANSI.Color) -> IORef T.Text -> Lock.Lock -> (T.Text -> ReaderT Env IO ()) -> ReaderT Env IO ()
basedOnChan_ color chan chanLock f =
  ask >>= liftIO . Lock.with chanLock . runReaderT (readFromChan color chan >>= f)

logText :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> ReaderT Env IO ()
logText (intensity,color) str = do
  env <- ask
  liftIO $ Lock.with (lock env) (do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground intensity color]
    T.putStrLn str
    ANSI.setSGR [ANSI.Reset])

send :: T.Text -> ReaderT Env IO ()
send str = do
  env <- ask
  liftIO . T.putStrLn $ "SENDING TO SERVER: " `T.append` str
  liftIO $ WS.sendTextData (conn env) str

sendWithColor :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> ReaderT Env IO ()
sendWithColor (intensity,color) str = do
  env <- ask
  liftIO $ Lock.with (lock env) (do
    ANSI.setSGR [ANSI.SetColor ANSI.Foreground intensity color]
    runReaderT (send str) env
    ANSI.setSGR [ANSI.Reset])

useGlobalCommand :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> ReaderT Env IO ()
useGlobalCommand color message = say color "global" message

sayInDefault :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> ReaderT Env IO ()
sayInDefault color message = say color Config.defaultRoom message

say :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> T.Text -> ReaderT Env IO ()
say color room message = sendWithColor color $ room `T.append` "|" `T.append` message

pm :: (ANSI.ColorIntensity, ANSI.Color) -> T.Text -> T.Text -> ReaderT Env IO ()
pm color username message = sendWithColor color $ "|/w " `T.append` username `T.append` "," `T.append` message

toId :: T.Text -> T.Text
toId = T.filter (\c -> c `elem` ['a'..'z'] || c `elem` ['0'..'9']) . T.toLower