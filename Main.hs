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
  _ <- forkIO $ forever $ WS.receiveData conn >>= liftIO . parse conn
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
          | "|pm" `T.isPrefixOf` s =
            let messageList = T.splitOn "|" s
            in  commandParse conn "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
          | otherwise = return ()
            where messageList = T.splitOn "|" s

commandParse :: WS.Connection -> T.Text -> T.Text -> T.Text -> IO ()
commandParse conn room username message
  | "should " `T.isPrefixOf` (T.toLower message) =
    if room == "pm"
      then send conn $ "|/w " `T.append` username `T.append` ", No."
      else send conn $ room `T.append` "|No."
  | "say " `T.isPrefixOf` (T.toLower message) && T.tail username == T.pack Config.username =
    send conn (T.tail.T.tail.T.tail.T.tail$message)    
  | otherwise = return ()


startUpActions :: WS.Connection -> IO ()
startUpActions conn = do
  send conn "|/blockchallenges"
  threadDelay $ 600 * 1000
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
