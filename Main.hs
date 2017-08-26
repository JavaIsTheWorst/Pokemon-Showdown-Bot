{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM.TMChan (newTMChanIO)
import           Control.Concurrent.MVar       (newMVar)
import           Control.Monad                 (forever, unless)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, runReaderT)
import           Network.Socket                (withSocketsDo)
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS

import qualified Commands
import qualified Config
import qualified Login
import qualified Plugins.Hangman               as Hangman
import qualified Plugins.Uno                   as Uno
import           Util

app :: WS.ClientApp ()
app conn' = do
  lock' <- newMVar ()
  unoChan' <- newTMChanIO
  unoChanLock' <- newMVar ()
  chessGame' <- newMVar Nothing
  gestiGame' <- newMVar Nothing
  let env = Env {lock=lock', conn=conn', unoChan=unoChan', unoChanLock=unoChanLock', chessGame=chessGame', gestiGame=gestiGame'}
  runReaderT (logText Config.connectedColor . T.pack $ "Successfully connected to " ++ "ws://" ++ Config.server ++ ":" ++ (show Config.port) ++ Config.path) env
  _ <- forkIO $ forever $ WS.receiveData conn' >>= (`runReaderT` env) . parse
  let loop = do
        line <- T.getLine
        unless (T.null line) $ runReaderT (sendWithColor Config.logColor line) env >> loop
  loop
  WS.sendClose conn' ("|/logout" :: T.Text)
  threadDelay $ 1000 * 1000

parse :: T.Text -> ReaderT Env IO ()
parse s = liftIO (T.putStrLn s) >> parse'
  where parse'
          | T.head s == '>' =
            if null$tail messageList
              then return ()
              else
                let room = T.init.T.tail$messageList !! 0
                in case head$tail messageList of
                  "c"     -> if (messageList !! 2) == "~"
                    then
                      if (messageList !! 3) == T.pack Config.username `T.append` "'s turn."
                        then Uno.onTurnMessage
                        else return ()
                    else Commands.parseMessage room (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  "c:"    -> if (messageList !! 3) == "~"
                    then
                      if (messageList !! 4) == T.pack Config.username `T.append` "'s turn."
                        then Uno.onTurnMessage
                        else return ()
                    else Commands.parseMessage room (messageList !! 3) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
                  "raw"   -> if "You have drawn the following card: " `T.isPrefixOf` (messageList !! 2)
                    then Uno.onDrawMessage $ messageList !! 2
                    else return ()
                  "uhtml" -> processHtmlResponse room (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  "uhtmlchange" -> processHtmlChange (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  _       -> return ()
          | T.head s /= '|' = return ()
          | "|challstr" `T.isPrefixOf` s =
            Login.onChallstrMessage s $ Commands.startUpActions
          | "|pm" `T.isPrefixOf` s =
            Commands.parseMessage "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
          | otherwise = return ()
            where messageList = T.splitOn "|" s
        processHtmlResponse :: T.Text -> T.Text -> T.Text -> ReaderT Env IO ()
        processHtmlResponse room messageType message
          | "hangman" `T.isPrefixOf` messageType = Hangman.onHangmanMessage room message
          | "uno-hand" == messageType = Uno.onUnoHandMessage room message
          | "uno" `T.isPrefixOf` messageType = Uno.onUnoMessage room message
          | otherwise = return ()
        processHtmlChange :: T.Text -> T.Text -> ReaderT Env IO ()
        processHtmlChange messageType message
          | "uno-hand" == messageType = Uno.onUnoHandChangeMessage
          | "uno" `T.isPrefixOf` messageType =
            if "The game of UNO has ended." `T.isInfixOf` message
              then Uno.onUnoEndMessage
              else return ()
          | otherwise = return ()

main :: IO ()
main = do
  setLocaleEncoding utf8
  withSocketsDo $ WS.runClient Config.server Config.port Config.path app