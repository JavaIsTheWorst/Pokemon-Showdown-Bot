{-# LANGUAGE OverloadedStrings #-}
module Commands where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.MVar       (MVar)
import           Control.Monad                 ((<=<))
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.List                     (intersperse, isPrefixOf)
import           System.Random                 (Random, random, randomR, randomRIO, randomIO)
import qualified Data.Text                     as T
import qualified Network.WebSockets            as WS
import qualified System.IO.Strict              as S

import           Commands.Settings
import qualified Config
import qualified Plugins.Chess                 as Chess
import           Util

data Choice = Yes | No deriving (Enum, Bounded)

instance Random Choice where
  random g =
    case randomR (fromEnum (minBound :: Choice), fromEnum (maxBound :: Choice)) g of
      (r,g') -> (toEnum r,g')
  randomR (a,b) g =
    case randomR (fromEnum a, fromEnum b) g of
      (r,g') -> (toEnum r,g')

instance Show Choice where
  show Yes = "Yes."
  show No = "No."

parseMessage :: T.Text -> T.Text -> T.Text -> ReaderT Env IO ()
parseMessage room username message
  | "should " `T.isPrefixOf` (T.toLower message) = do
    env <- ask
    liftIO $ checkSetting "should auto-reply no" (T.unpack room) (==Just "True")
      (if room == "pm"
        then runReaderT (pm Config.commandColor username "No.") env
        else runReaderT (say Config.commandColor room "No.") env) $ return ()
  | "stop saying no in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should auto-reply no" (T.unpack . T.drop (T.length "stop saying no in ") . snd . T.breakOn "stop saying no in " $ T.toLower message) "False"
  | "start saying no in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should auto-reply no" (T.unpack . T.drop (T.length "start saying no in ") . snd . T.breakOn "start saying no in " $ T.toLower message) "True"
  | "say " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let unEscape = read . T.unpack . ("\"" `T.append`) . (`T.append` "\"") . T.replace "\"" "\\\""
    in sendWithColor Config.commandColor $ unEscape (T.tail.T.tail.T.tail.T.tail$message)
  | "github please" `T.isInfixOf` (T.toLower message) =
    pm Config.commandColor username "https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"
  | "what is java" `T.isPrefixOf` (T.toLower message) = do
    env <- ask
    liftIO . forkIO $ do
      runReaderT (pm Config.commandColor username "Java is a general-purpose (aka vaguely defined purpose) computer programming language (voodoo) that is concurrent (bad at multi-tasking), class-based (tries to be well structured), object-oriented (cannot process abstract ideas), and specifically designed to have as few implementation dependencies") env
      runReaderT (pm Config.commandColor username "as possible (isolated from reality).") env
      threadDelay $ 1*1000*1000
      runReaderT (pm Config.commandColor username "It is intended to let application developers \"write once, run anywhere\", meaning that compiled Java code can run on all platforms that support Java without the need for recompilation") env
      runReaderT (pm Config.commandColor username "(making the arms of corporate conspiracies to quietly take over the modern society stretch all that farther).") env
    return ()
  | "ab" == message =
    pm Config.commandColor username $ "Identity confirmed: " `T.append` T.tail username
  | "in this room, " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let parts = T.splitOn " -> " $ T.drop (T.length "in this room, ") message
    in if null $ tail parts
         then return ()
         else do
           commands <- liftIO $ S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack room ++ " -> " ++ T.unpack message ++ " -> ")`isPrefixOf`)) $ lines commands
               updatedCommands = unlines $ (T.unpack $ T.intercalate " -> " [room, head parts, head $ tail parts]) : filteredCommandList
           liftIO $ writeFile "roomcommands.txt" updatedCommands
  | "delete relation " `T.isPrefixOf` (T.toLower message) =
    let parts = T.splitOn " -> " $ T.drop (T.length "delete relation ") message
    in if null $ tail parts
         then return ()
         else do
           commands <- liftIO $ S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack $ T.intercalate " -> " [room, head parts, head $ tail parts])==)) $ lines commands
           liftIO . writeFile "roomcommands.txt" $ unlines filteredCommandList
  | "clear room commands" `T.isInfixOf` (T.toLower message) =
    liftIO $ writeFile "roomcommands.txt" ""
  | (toId (T.pack Config.username) `T.append` "choose") `T.isPrefixOf` (toId message) = do
    env <- ask
    liftIO $ checkSetting "should choose" (T.unpack room) (==Just "True") (do
      choice <- randomIO :: IO Choice
      let textChoice = T.pack $ show choice
      if room == "pm"
        then runReaderT (pm Config.commandColor username textChoice) env
        else runReaderT (say Config.commandColor room textChoice) env) $ return ()
  | "stop choosing in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should choose" (T.unpack . T.drop (T.length "stop choosing in ") . snd . T.breakOn "stop choosing in " $ T.toLower message) "False"
  | "start choosing" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should choose" (T.unpack . T.drop (T.length "start choosing in ") . snd . T.breakOn "start choosing in " $ T.toLower message) "True"
  | "vinegar garbodor with an aside of trubbish" == message =
    pm Config.commandColor username "vinegar trubbish with an aside of garbodor"
  | "can i" `T.isInfixOf` (T.toLower message) || "can we" `T.isInfixOf` (T.toLower message) = do
    env <- ask
    liftIO $ checkSetting "should can I auto-reply" (T.unpack room) (==Just "True")
      (if room == "pm"
        then runReaderT (pm Config.commandColor username "No, you cannot.") env
        else runReaderT (say Config.commandColor room "No, you cannot.") env) $ return ()
  | "start telling people what they can do in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should can I auto-reply" (T.unpack . T.drop (T.length "start telling people what they can do in ") . snd . T.breakOn "start telling people what they can do in " $ T.toLower message) "True"
  | "stop telling people what they can do in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should can I auto-reply" (T.unpack . T.drop (T.length "stop telling people what they can do in ") . snd . T.breakOn "stop telling people what they can do in " $ T.toLower message) "False"
{-
  | "initiate chess in " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) = do
    let room = T.drop (T.length "initiate chess in ") . fst $ T.breakOn ":" message
        players = snd $ T.breakOn ":" message
        playerOne = toId . fst $ T.breakOn "," players
        playerTwo = toId . snd $ T.breakOn "," players
    in Chess.onInitiateMessage lock conn room playerOne playerTwo
-}
  | otherwise =
    if room == "pm" && toId username /= toId (T.pack Config.username)
      then do
        env <- ask
        liftIO . forkIO $ do
          respond <- randomRIO (1,5) :: IO Int
          if respond == 1
            then return ()
            else do
              m <- randomRIO (1,20) :: IO Int
              threadDelay $ m*1000*1000
              n <- randomRIO (1,4) :: IO Int
              runReaderT (pm Config.commandColor username $ T.replicate n "?") env
        return ()
      else do
        commands <- liftIO $ S.readFile "roomcommands.txt"
        let searchedCommand = filter ((T.unpack room ++ " -> " ++ T.unpack (T.toLower message) ++ " -> ")`isPrefixOf`) $ lines commands
        if null searchedCommand
          then return ()
          else
            let unEscape         = read . T.unpack . ("\"" `T.append`) . (`T.append` "\"") . T.replace "\"" "\\\""
                splitText        = T.splitOn "$(pick)"
                processRandoms s =
                  let choices = splitText s
                  in if length choices == 1
                    then return s
                    else (choices !!) <$> (liftIO $ randomRIO (1,length choices))
            in  sendWithColor Config.commandColor <=< processRandoms . T.replace "$(user)" username . unEscape . head.tail.tail . T.splitOn " -> " . T.pack $ head searchedCommand

startUpActions :: ReaderT Env IO ()
startUpActions = do
  env <- ask
  liftIO . forkIO $ do
    runReaderT (useCommand Config.startUpColor "/avatar 120") env
    threadDelay $ 600 * 1000
    let joinsReaderT   = map (useCommand Config.startUpColor . ("/j " `T.append`)) Config.rooms
        joins          = map (($ env) . runReaderT) joinsReaderT
        joinsWithDelay = intersperse (threadDelay $ 600 * 1000) joins
    sequence_ joinsWithDelay
    threadDelay $ 600 * 1000
    let pmsReaderT     = map (($ "Harassed.") . pm Config.startUpColor) Config.harassList
        pms            = map (($ env) . runReaderT) pmsReaderT
        pmsWithDelay   = intersperse (threadDelay $ 600 * 1000) pmsWithDelay
    sequence_ pmsWithDelay
  return ()
