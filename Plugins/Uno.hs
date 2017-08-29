{-# LANGUAGE OverloadedStrings #-}
module Plugins.Uno where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.IORef                    (modifyIORef, readIORef, newIORef)
import           Data.List                     (group, maximumBy, sort)
import           Data.Ord                      (comparing)
import qualified Control.Concurrent.Lock       as Lock
import qualified Data.Map.Strict               as Map
import qualified Data.Text                     as T

import qualified Config
import           Plugins.Uno.Types
import           Util

onUnoMessage :: T.Text -> ReaderT Env IO ()
onUnoMessage room = do
  env <- ask
  say Config.unoColor room "/uno join"
  chan' <- liftIO $ newIORef ""
  logChan Config.unoColor chan' "UNO: Joined game"
  chanLock' <- liftIO $ Lock.new
  liftIO . modifyIORef (unoGames env) $ Map.insert room (UNOGame {chan=chan', chanLock=chanLock'})

onTurnMessage :: T.Text -> ReaderT Env IO ()
onTurnMessage room = do
  maybeUnoGame <- fmap (Map.lookup room) . liftIO . readIORef . unoGames =<< ask
  case maybeUnoGame of
    Just unoGame -> logChan Config.unoColor (chan unoGame) "UNO: My turn"
    _            -> return ()

onDrawMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onDrawMessage room message = do
  maybeUnoGame <- fmap (Map.lookup room) . liftIO . readIORef . unoGames =<< ask
  case maybeUnoGame of
    Just unoGame -> logChan Config.unoColor (chan unoGame) $
      let card = T.tail . fst . T.breakOn "<" . snd $ T.breakOn ">" message
      in "UNO: Drew " `T.append` card
    _            -> return ()

onUnoHandMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoHandMessage room message = do
  env <- ask
  maybeUnoGame <- fmap (Map.lookup room) . liftIO . readIORef $ unoGames env
  case maybeUnoGame of
    Just unoGame -> do
      _ <- liftIO . forkIO $ runReaderT (do
      let cards = map (readUNOCard . T.drop (T.length "/uno play ")) . filter ("/uno play " `T.isPrefixOf`) $ T.splitOn "\"" message
      if null cards
        then return ()
        else
          let process chanMessage
                | "UNO hand display change" == chanMessage = do
                  if length cards == 1
                    then do
                      let unoCommand = head . filter ("/uno uno " `T.isPrefixOf`) $ T.splitOn "\"" message
                      say Config.unoColor room unoCommand
                      logChan Config.unoColor (chan unoGame) "UNO: Saying UNO"
                    else logChan Config.unoColor (chan unoGame) "UNO hand display change acknowledged"
                | "UNO: Drew" `T.isPrefixOf` chanMessage =
                  logChan Config.unoColor (chan unoGame) chanMessage
                | otherwise = do
                    logChan Config.unoColor (chan unoGame) "UNO: Making my decision"
                    logText Config.unoColor . ("UNO CARDS: " `T.append`) . T.pack . show $ map cardToText cards
                    let topCard = readUNOCard (fst $ T.breakOn "<" (T.tail . snd $ T.breakOn ">" (snd $ T.breakOn "Top Card: " message)))
                        topCardAccountingWild = if color topCard == UNOWild
                          then
                            let rgb = T.takeWhile (/= '\"') . T.drop (T.length "Top Card: <span style=\"color: ") . snd $ T.breakOn "Top Card: <span style=\"color: " message
                            in UNOCard {color = rgbToUNOColor rgb, number = None}
                          else topCard
                        wildCards = filter ((== UNOWild) . color) cards
                        nonWildCards = filter ((/= UNOWild) . color) cards
                        nonWildPlayableCards = filter (\card -> color card == color topCardAccountingWild || number card == number topCardAccountingWild) nonWildCards
                    if null nonWildPlayableCards
                      then
                        if null wildCards
                          then do
                            say Config.unoColor room "/uno draw"
                            let loop = do
                                  liftIO $ threadDelay $ 800 * 1000
                                  basedOnChan Config.unoColor (chan unoGame) (chanLock unoGame) (\chanMessage' ->
                                      if "UNO: Drew " `T.isPrefixOf` chanMessage'
                                        then do
                                          logChan Config.unoColor (chan unoGame) "UNO: Received draw message"
                                          return $ T.drop (T.length "UNO: Drew ") chanMessage'
                                        else loop)
                            drawnCardText <- loop
                            let drawnCard = readUNOCard drawnCardText
                            if color drawnCard == UNOWild
                              then do
                                let modeColor = if null nonWildCards
                                      then UNOBlue
                                      else head . maximumBy (comparing length) . group . sort $ map color nonWildCards
                                say Config.unoColor room $ "/uno play " `T.append` cardToText drawnCard
                                liftIO . threadDelay $ 600 * 1000
                                say Config.unoColor room $ "/uno color " `T.append` colorToText modeColor
                              else
                                if color drawnCard == color topCardAccountingWild || number drawnCard == number topCardAccountingWild
                                  then say Config.unoColor room $ "/uno play " `T.append` cardToText drawnCard
                                  else say Config.unoColor room "/uno pass"
                          else do
                            let modeColor = if null nonWildCards
                                  then UNOBlue
                                  else head . maximumBy (comparing length) . group . sort $ map color nonWildCards
                            say Config.unoColor room $ "/uno play " `T.append` cardToText (head wildCards)
                            liftIO . threadDelay $ 600 * 1000
                            say Config.unoColor room $ "/uno color " `T.append` colorToText modeColor
                      else say Config.unoColor room $ "/uno play " `T.append` cardToText (head nonWildPlayableCards)
          in readFromChan Config.unoColor (chan unoGame) >>= process) env
      return ()
    _         -> return ()

onUnoHandDisplayChangeMessage :: T.Text -> ReaderT Env IO ()
onUnoHandDisplayChangeMessage room = do
  maybeUnoGame <- fmap (Map.lookup room) . liftIO . readIORef . unoGames =<< ask
  case maybeUnoGame of
    Just unoGame ->
      let process chanMessage
            | "UNO: My turn" == chanMessage || "UNO: Drew " `T.isPrefixOf` chanMessage =
                logChan Config.unoColor (chan unoGame) chanMessage
            | otherwise                               =
                logChan Config.unoColor (chan unoGame) "UNO hand display change"
      in basedOnChan_ Config.unoColor (chan unoGame) (chanLock unoGame) process
    _            -> return ()

onUnoEndMessage :: T.Text -> ReaderT Env IO ()
onUnoEndMessage room = do
  env <- ask
  liftIO . modifyIORef (unoGames env) $ Map.delete room