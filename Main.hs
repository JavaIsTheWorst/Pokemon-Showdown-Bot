{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import           Control.Concurrent           (forkIO, threadDelay)
import           Network.HTTP                 (simpleHTTP, postRequestWithBody, getResponseBody)
import           Network.HTTP.Base            (urlEncode, rspReason)
import           Network.Socket               (withSocketsDo)
import           Control.Monad                (forever, unless)
import           Control.Monad.Trans          (liftIO)
import           Data.Aeson                   (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Data.List                    (isPrefixOf, isInfixOf)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           GHC.IO.Encoding              (setLocaleEncoding, utf8)
import           Data.Text.Encoding           (encodeUtf8)
import           System.Random                (Random, random, randomR, randomRIO, randomIO)
import qualified System.IO.Strict             as S
import qualified Data.ByteString.Lazy         as B
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import qualified Network.WebSockets           as WS

import qualified Config
import           Util

app :: WS.ClientApp ()
app conn = do
  putStrLn $ "Successfully connected to " ++ "ws://" ++ Config.server ++ ":" ++ (show Config.port) ++ Config.path
  _ <- forkIO $ forever $ WS.receiveData conn >>= parse conn
  let loop = do
        line <- T.getLine
        unless (T.null line) $ send conn line >> loop
  loop
  WS.sendClose conn ("|/logout" :: Text)
  threadDelay $ 1000 * 1000

parse :: WS.Connection -> T.Text -> IO ()
parse conn s = print s >> parse' conn s
  where parse' conn s
          | T.head s == '>' =
            if null$tail messageList
              then return ()
              else case head$tail messageList of
                "c"  -> commandParse conn (T.init.T.tail$messageList !! 0) (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                "c:" -> commandParse conn (T.init.T.tail$messageList !! 0) (messageList !! 3) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
                _    -> return ()
          | T.head s /= '|' = return ()
          | "|challstr" `T.isPrefixOf` s =
            let postData = "act=login&name=" ++ urlEncode Config.username ++ "&pass=" ++ urlEncode Config.password ++ "&challstr=" ++ urlEncode (T.unpack $ T.drop 10 s)
            in  do
              httpResponse <- simpleHTTP (postRequestWithBody Config.actionUrl "application/x-www-form-urlencoded" postData)
              (_:jsonData) <- getResponseBody httpResponse
              let loginCommand = "/trn " ++ Config.username ++ ",0," ++ getAssertion jsonData
              useCommand conn $ T.pack loginCommand
              startUpActions conn
          | "|pm" `T.isPrefixOf` s = commandParse conn "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
          | otherwise = return ()
            where messageList = T.splitOn "|" s

setSetting :: String -> String -> IO ()
setSetting setting newSetting = do
  settings <- S.readFile "settings.txt"
  let filteredSettingsList = filter (not . (setting `isPrefixOf`)) $ lines settings
  let updatedSettings = unlines $ (setting ++ ": " ++ newSetting) : filteredSettingsList
  writeFile "settings.txt" updatedSettings

getSetting :: String -> IO (Maybe String)
getSetting setting = do
  settings <- S.readFile "settings.txt"
  let searchedSetting = filter (setting `isPrefixOf`) $ lines settings
  if null searchedSetting
    then return Nothing
    else return . Just . drop (length setting + 2) . head $ searchedSetting

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

commandParse :: WS.Connection -> T.Text -> T.Text -> T.Text -> IO ()
commandParse conn room username message
  | "should " `T.isPrefixOf` (T.toLower message) = do
    setting <- getSetting "should auto-reply no"
    if setting == Nothing || setting /= Just "True"
      then return ()
      else if room == "pm"
        then pm conn username "No."
        else say conn room "No."
  | "stop saying no" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should auto-reply no" "False"
  | "start saying no" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should auto-reply no" "True"
  | "say " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    send conn (T.tail.T.tail.T.tail.T.tail$message)
  | "github please" `T.isInfixOf` (T.toLower message) =
    pm conn username "https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"
  | "what is java" `T.isPrefixOf` (T.toLower message) = do
    forkIO $ do
      pm conn username "Java is a general-purpose (aka vaguely defined purpose) computer programming language (voodoo) that is concurrent (bad at multi-tasking), class-based (tries to be well structured), object-oriented (cannot process abstract ideas), and specifically designed to have as few implementation dependencies"
      pm conn username "as possible (isolated from reality)."
      threadDelay $ 1*1000*1000
      pm conn username "It is intended to let application developers \"write once, run anywhere\", meaning that compiled Java code can run on all platforms that support Java without the need for recompilation"
      pm conn username "(making the arms of corporate conspiracies to quietly take over the modern society stretch all that farther)."
    return ()
  | "ab" == message =
    pm conn username $ ", Identity confirmed: " `T.append` username
  | "in this room, " `T.isPrefixOf` (T.toLower message) =
    let parts = T.splitOn " -> " $ T.drop (T.length "in this room, ") message
    in if null $ tail parts
         then return ()
         else do
           commands <- S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack room ++ " -> " ++ T.unpack message ++ " -> ")`isPrefixOf`)) $ lines commands
           let updatedCommands = unlines $ (T.unpack $ T.intercalate " -> " [room, head parts, head $ tail parts]) : filteredCommandList
           writeFile "roomcommands.txt" updatedCommands
  | "delete relation " `T.isPrefixOf` (T.toLower message) =
    let parts = T.splitOn " -> " $ T.drop (T.length "delete relation ") message
    in if null $ tail parts
         then return ()
         else do
           commands <- S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack $ T.intercalate " -> " [room, head parts, head $ tail parts])==)) $ lines commands
           writeFile "roomcommands.txt" $ unlines filteredCommandList
  | (toId (T.pack Config.username) `T.append` ", choose") `T.isPrefixOf` (T.toLower message) = do
    setting <- getSetting "should choose"
    if setting == Nothing || setting /= Just "True"
      then return ()
      else do
        choice <- randomIO :: IO Choice
        let textChoice = T.pack $ show choice
        if room == "pm"
          then pm conn username textChoice
          else say conn room textChoice
  | "stop choosing" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should choose" "False"
  | "start choosing" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should choose" "True"
  | "vinegar garbodor with an aside of trubbish" == username =
    pm conn username "vinegar trubbish with an aside of garbodor"
  | otherwise =
    if room == "pm" && toId username /= toId (T.pack Config.username)
      then do
        forkIO $ do
          respond <- randomRIO (1,5) :: IO Int
          if respond == 1
            then return ()
            else do
              m <- randomRIO (1,20) :: IO Int
              threadDelay $ m*1000*1000
              n <- randomRIO (1,4) :: IO Int
              pm conn username $ T.replicate n "?"
        return ()
      else do
        commands <- S.readFile "roomcommands.txt"
        let searchedCommand = filter ((T.unpack room ++ " -> " ++ T.unpack message ++ " -> ")`isPrefixOf`) $ lines commands
        if null searchedCommand
          then return ()
          else say conn room . head.tail.tail . T.splitOn " -> " . T.pack $ head searchedCommand

startUpActions :: WS.Connection -> IO ()
startUpActions conn = do
  useCommand conn "/avatar 120"
  threadDelay $ 600 * 1000
  mapM_ ((>> threadDelay (600 * 1000)) . useCommand conn . ("/j " `T.append`)) Config.rooms
  mapM_ ((>> threadDelay (600 * 1000)) . ($ "Harassed.") . pm conn) Config.harassList

data User = User {
  userid :: String,
  usernum :: String,
  username :: String,
  email :: Maybe String,
  registertime :: String,
  group :: String,
  banstate :: String,
  ip :: String,
  avatar :: String,
  account :: Maybe String,
  logintime :: String,
  loginip :: String,
  loggedin :: Bool,
  outdatedpassword :: Bool
  } deriving (Generic, Show)

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON User

data AssertionObject = AssertionObject {
  curuser :: User,
  actionsuccess :: Bool,
  assertion :: String
  } deriving (Generic, Show)

instance ToJSON AssertionObject where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AssertionObject

getAssertion :: String -> String
getAssertion = assertion . either error id . eitherDecode . toLazyByteString
  where toLazyByteString = B.fromStrict . encodeUtf8 . T.pack

main = do
  setLocaleEncoding utf8
  withSocketsDo $ WS.runClient Config.server Config.port Config.path app
