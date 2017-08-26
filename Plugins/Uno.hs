{-# LANGUAGE OverloadedStrings #-}
module Plugins.Uno where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.List                     (group, maximumBy, sort)
import           Data.Ord                      (comparing)
import qualified Data.Text                     as T

import           Commands.Settings
import qualified Config
import           Plugins.Uno.Types
import           Util

onUnoMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoMessage room _ = do
  env <- ask
  liftIO $ checkSetting "in uno game" "default" (/=Just "True") (do
    runReaderT (say Config.unoColor room "/uno join") env
    runReaderT (logChan Config.unoColor (unoChan env) "UNO: Joined game") env) $ return ()
  liftIO $ setSetting "in uno game" "default" "True"

onTurnMessage :: ReaderT Env IO ()
onTurnMessage = do
  env <- ask
  basedOnChan_ Config.unoColor (unoChan env) (unoChanLock env) (\_ ->
    logChan Config.unoColor (unoChan env) "UNO: My turn")

onDrawMessage :: T.Text -> ReaderT Env IO ()
onDrawMessage message = do
  env <- ask
  basedOnChan_ Config.unoColor (unoChan env) (unoChanLock env) (\_ ->
    let card = T.tail . fst . T.breakOn "<" . snd $ T.breakOn ">" message
    in logChan Config.unoColor (unoChan env) $ "UNO: Drew " `T.append` card)

onUnoHandMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onUnoHandMessage room message = do
  env <- ask
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
                    logChan Config.unoColor (unoChan env) "UNO: Saying UNO"
                  else logChan Config.unoColor (unoChan env) "UNO hand display change acknowledged"
              | "UNO: Drew" `T.isPrefixOf` chanMessage =
                logChan Config.unoColor (unoChan env) chanMessage
              | otherwise = do
                  logChan Config.unoColor (unoChan env) "UNO: Making my decision"
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
                                basedOnChan Config.unoColor (unoChan env) (unoChanLock env) (\maybeChanMessage' ->
                                  case maybeChanMessage' of
                                    Just chanMessage' -> if "UNO: Drew " `T.isPrefixOf` chanMessage'
                                      then do
                                        logChan Config.unoColor (unoChan env) "UNO: Received draw message"
                                        return $ T.drop (T.length "UNO: Drew ") chanMessage'
                                      else do
                                        logChan Config.unoColor (unoChan env) chanMessage'
                                        loop
                                    _                -> return "")
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
        in readFromChan Config.unoColor (unoChan env) >>= (\maybeChanMessage ->
              case maybeChanMessage of 
                Just chanMessage -> process chanMessage
                _                -> return ())) env
  return ()

onUnoHandChangeMessage :: ReaderT Env IO ()
onUnoHandChangeMessage = do
  env <- ask
  let process chanMessage
        | "UNO: My turn" == chanMessage           =
            logChan Config.unoColor (unoChan env) chanMessage
        | "UNO: Drew " `T.isPrefixOf` chanMessage =
            logChan Config.unoColor (unoChan env) chanMessage
        | otherwise                               =
            logChan Config.unoColor (unoChan env) "UNO hand display change"
      changeMessage (Just message) = process message
      changeMessage _              = return ()
  basedOnChan_ Config.unoColor (unoChan env) (unoChanLock env) changeMessage

onUnoEndMessage :: ReaderT Env IO ()
onUnoEndMessage = do
  env <- ask
  basedOnChan_ Config.unoColor (unoChan env) (unoChanLock env) (\_ -> return ())
  liftIO $ setSetting "in uno game" "default" "False"