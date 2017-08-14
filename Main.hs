{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import           Control.Concurrent   (forkIO, threadDelay)
import           Network.HTTP         (simpleHTTP, postRequestWithBody, getResponseBody)
import           Network.HTTP.Base    (urlEncode, rspReason)
import           Network.Socket       (withSocketsDo)
import           Control.Monad        (forever, unless)
import           Control.Monad.Trans  (liftIO)
import           Data.Aeson           (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Data.List            (isPrefixOf)
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
import           Data.Text.Encoding   (encodeUtf8)
import           System.Random        (randomRIO)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T
import qualified Network.WebSockets   as WS

import qualified Config

send :: WS.Connection -> T.Text -> IO ()
send = WS.sendTextData

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
              let loginCommand = "|/trn " ++ Config.username ++ ",0," ++ getAssertion jsonData
              send conn $ T.pack loginCommand
              startUpActions conn
          | "|pm" `T.isPrefixOf` s = do
            appendFile "log.txt" $ T.unpack s
            commandParse conn "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
          | otherwise = return ()
            where messageList = T.splitOn "|" s

commandParse :: WS.Connection -> T.Text -> T.Text -> T.Text -> IO ()
commandParse conn room username message
  | "should " `T.isPrefixOf` (T.toLower message) =
    if room == "pm"
      then send conn $ "|/w " `T.append` username `T.append` ", No."
      else send conn $ room `T.append` "|No."
  | "say " `T.isPrefixOf` (T.toLower message) && T.tail username == T.pack Config.owner =
    send conn (T.tail.T.tail.T.tail.T.tail$message)
  | "github please" `T.isPrefixOf` (T.toLower message) =
    send conn $ "|/w " `T.append` username `T.append` ", https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"
  | "what is java" `T.isPrefixOf` (T.toLower message) = do
    forkIO $ do
      send conn $ "|/w " `T.append` username `T.append` ", Java is a general-purpose (aka vaguely defined purpose) computer programming language (voodoo) that is concurrent (bad at multi-tasking), class-based (tries to be well structured), object-oriented (cannot process abstract ideas), and specifically designed to have as few implementation dependencies"
      send conn $ "|/w " `T.append` username `T.append` ", as possible (isolated from reality)."
      threadDelay $ 1*1000*1000
      send conn $ "|/w " `T.append` username `T.append` ", It is intended to let application developers \"write once, run anywhere\", meaning that compiled Java code can run on all platforms that support Java without the need for recompilation"
      send conn $ "|/w " `T.append` username `T.append` ", (making the arms of corporate conspiracies to quietly take over the modern society stretch all that farther)."
    return ()
  | "ab" == message =
    send conn $ "|/w " `T.append` username `T.append` ", Identity confirmed: " `T.append` username
  | otherwise =
    if room == "pm" && T.tail username /= T.pack Config.username
      then do
        forkIO $ do
          m <- randomRIO (1,20) :: IO Int
          threadDelay $ m*1000*1000
          n <- randomRIO (1,4) :: IO Int
          send conn $ "|/w " `T.append` username `T.append` "," `T.append` T.replicate n "?"
        return ()
      else return ()

startUpActions :: WS.Connection -> IO ()
startUpActions conn = do
  send conn "|/avatar 120"
  threadDelay $ 600 * 1000
  mapM_ ((>> threadDelay (600 * 1000)) . send conn . T.append "|/j ") Config.rooms
  mapM_ ((>> threadDelay (600 * 1000)) . send conn . (flip T.append) ", Harassed." . T.append "|/w ") Config.harassList

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
