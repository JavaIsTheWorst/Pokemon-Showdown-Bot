{-# LANGUAGE OverloadedStrings #-}
module Plugins.Uno where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.MVar       (MVar)
import           Control.Concurrent.STM.TMChan (TMChan)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.List                     (group, maximumBy, sort)
import           Data.Ord                      (comparing)
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS

import           Commands.Settings
import qualified Config
import           Plugins.Uno.Types
import           Util

onUnoMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoMessage room _ = do
  env <- ask
  liftIO $ checkSetting "in uno game" "default" (/=Just "True") (do
    runReaderT (say Config.unoColor room "/uno join") env
    runReaderT (logChan Config.unoColor "UNO: Joined game") env) $ return ()
  liftIO $ setSetting "in uno game" "default" "True"

onTurnMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onTurnMessage _ _ =
  basedOnChan_ Config.unoColor (\_ ->
    logChan Config.unoColor "UNO: My turn")

onDrawMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onDrawMessage _ message =
  basedOnChan_ Config.unoColor (\_ ->
    let card = T.tail . fst . T.breakOn "<" . snd $ T.breakOn ">" message
    in logChan Config.unoColor $ "UNO: Drew " `T.append` card)

onUnoHandMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoHandMessage room message = do
  env <- ask
  liftIO . forkIO $ runReaderT (do
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
                    logChan Config.unoColor "UNO: Saying UNO"
                  else logChan Config.unoColor "UNO hand display change acknowledged"
              | "UNO: Drew" `T.isPrefixOf` chanMessage =
                logChan Config.unoColor chanMessage
              | otherwise = do
                  logChan Config.unoColor "UNO: Making my decision"
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
                                logText Config.unoColor "Loop"
                                basedOnChan Config.unoColor (\chanMessage ->
                                  case chanMessage of
                                    Just message -> if "UNO: Drew " `T.isPrefixOf` message
                                      then do
                                        logChan Config.unoColor "UNO: Received draw message"
                                        return $ T.drop (T.length "UNO: Drew ") message
                                      else do
                                        logChan Config.unoColor message
                                        loop
                                    _            -> return "")
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
        in  readFromChan Config.unoColor >>= (\chanMessage ->
              case chanMessage of 
                Just message -> process message
                _            -> return ())) env
  return ()

onUnoHandChangeMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoHandChangeMessage _ _ =
  let process chanMessage
        | "UNO: My turn" == chanMessage           =
            logChan Config.unoColor chanMessage
        | "UNO: Drew " `T.isPrefixOf` chanMessage =
            logChan Config.unoColor chanMessage
        | otherwise                               =
            logChan Config.unoColor "UNO hand display change"
      changeMessage (Just message) = process message
      changeMessage _              = return ()
  in basedOnChan_ Config.unoColor changeMessage

onUnoEndMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoEndMessage _ _ = do
  basedOnChan_ Config.unoColor (\_ -> return ())
  liftIO $ setSetting "in uno game" "default" "False"
