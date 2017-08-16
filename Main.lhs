> {-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
> module Main where

> import           Control.Concurrent   (forkIO, threadDelay)
> import           Network.HTTP         (simpleHTTP, postRequestWithBody, getResponseBody)
> import           Network.HTTP.Base    (urlEncode, rspReason)
> import           Network.Socket       (withSocketsDo)
> import           Control.Monad        (forever, unless)
> import           Control.Monad.Trans  (liftIO)
> import           Data.Aeson           (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
> import           Data.List            (isPrefixOf)
> import           Data.Text            (Text)
> import           GHC.Generics         (Generic)
> import           GHC.IO.Encoding      (setLocaleEncoding, utf8)
> import           Data.Text.Encoding   (encodeUtf8)
> import qualified Data.ByteString.Lazy as B
> import qualified Data.Text            as T
> import qualified Data.Text.IO         as T
> import qualified Network.WebSockets   as WS

> import qualified Config

Define a convenience function so we don't have to keep typing out WS.sendTextData

> send :: WS.Connection -> T.Text -> IO ()
> send = WS.sendTextData

Define what the bot should do

> app :: WS.ClientApp ()
> app conn = do

Log that we've successfully connected to the Pokemon Showdown! Websocket to stdout (If this function has been called, then we've already connected)

>   putStrLn $ "Successfully connected to " ++ "ws://" ++ Config.server ++ ":" ++ (show Config.port) ++ Config.path

Create a thread to read and parse information from the websocket continuously (until the program ends)

>   _ <- forkIO $ forever $ WS.receiveData conn >>= liftIO . parse conn

Create a loop to read from stdin and send whatever text the user types into stdin to the Pokemon Showdown! Websocket.
If the user enters an empty line, the bot should send a logout request to the Pokemon Showdown! server through the Websocket, wait a second, and then exit.

>   let loop = do
>         line <- T.getLine
>         unless (T.null line) $ send conn line >> loop
>   loop
>   WS.sendClose conn ("|/logout" :: Text)
>   threadDelay $ 1000 * 1000

Parse a message from the server (i.e. decide what the bot should do based on the message)

> parse :: WS.Connection -> T.Text -> IO ()
> parse conn s = print s >> parse' conn s
>   where parse' conn s

If the message starts with the character '>', this is a room message.
If the message does not contain a | character, that means it is not a user chat message, and we can ignore it.
If the message is in the structure ">room\n|c|user|message" or ">room\n|c:|timestamp|user|message", then we send it to commandParse with the connection, the room, the user, and the message.

>           | T.head s == '>' =
>             if null$tail messageList
>               then return ()
>               else case head$tail messageList of
>                 "c"  -> commandParse conn (T.init.T.tail$messageList !! 0) (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
>                 "c:" -> commandParse conn (T.init.T.tail$messageList !! 0) (messageList !! 3) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
>                 _    -> return ()

Ignore any messages that do not start with '|' (besides messages that start with '>' which we have a rule for).

>           | T.head s /= '|' = return ()

If the message starts with "|challstr", then this is part of the login process. The part of the message after "|challstr|" is the CHALLSTR.
The bot should in response to this message, POST a request to the PS! action url with the information "act=login&name=(url encoded username)&pass=(url encoded password)&challstr=(url encoded CHALLSTR)"
After the server responds to this POST request, the bot should take the response (which should be in the form "]data", where data is a JSON object containing an assertion).
With the response, it will remove the leading ']' character and give data to the getAssertion function, which will extract the assertion from data.
Given the assertion, the bot should then send to the server "|/trn username,0,assertion" where username is the username in Config and assertion is the assertion just obtained.
It should then run the startUpActions command, containing all the actions the bot should do upon logging in.

>           | "|challstr" `T.isPrefixOf` s =
>             let postData = "act=login&name=" ++ urlEncode Config.username ++ "&pass=" ++ urlEncode Config.password ++ "&challstr=" ++ urlEncode (T.unpack $ T.drop 10 s)
>             in  do
>               httpResponse <- simpleHTTP (postRequestWithBody Config.actionUrl "application/x-www-form-urlencoded" postData)
>               (_:jsonData) <- getResponseBody httpResponse
>               let loginCommand = "|/trn " ++ Config.username ++ ",0," ++ getAssertion jsonData
>               send conn $ T.pack loginCommand
>               startUpActions conn

If the message starts with "|pm", it is a PM (either from a user to the bot or the bot to a user).
In this case, we extract the user that PMed and user's message from the Websocket message (from the form |pm|user|user2|message)

>           | "|pm" `T.isPrefixOf` s =
>             let messageList = T.splitOn "|" s
>             in  commandParse conn "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)

If the message starts with anything else, do nothing.

>           | otherwise = return ()

>             where messageList = T.splitOn "|" s

Parse messages from PS! users in the following way.

> commandParse :: WS.Connection -> T.Text -> T.Text -> T.Text -> IO ()
> commandParse conn room username message

If the message starts with "should ", ignoring case, then if the user PMed it, the bot should PM the reply "No." back.
Otherwise, the bot should say in the room that the user said it "No."

>   | "should " `T.isPrefixOf` (T.toLower message) =
>     if room == "pm"
>       then send conn $ "|/w " `T.append` username `T.append` ", No."
>       else send conn $ room `T.append` "|No."

If the message starts with "say ", ignoring case, AND the user saying it is the bot's owner, then the bot should send to the server the user's message (dropping the "say " prefix).

>   | "say " `T.isPrefixOf` (T.toLower message) && T.tail username == T.pack Config.owner =
>     send conn (T.tail.T.tail.T.tail.T.tail$message)

If the message starts with "github please", ignoring case, then the bot should PM the user the Github link for the bot.

>   | "github please" `T.isPrefixOf` (T.toLower message) =
>     send conn $ "|/w " `T.append` username `T.append` ", https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"

If the message does not fit any of these conditions, do nothing.

>   | otherwise = return ()


Define the startup actions.

> startUpActions :: WS.Connection -> IO ()
> startUpActions conn = do

Send the server "|/blockchallenges" to block all challenge requests.

>   send conn "|/blockchallenges"

Wait 0.6 seconds so we don't get stopped automatically by the server for typing too quickly.

>   threadDelay $ 600 * 1000

Send the server "|/avatar 120" to set the bot's avatar.

>   send conn "|/avatar 120"

Again, wait 0.6 seconds.

>   threadDelay $ 600 * 1000

Send the server a list of commands in the form "|/j room" where room is one of the rooms from the room auto-join list. Wait 0.6 seconds after each command.

>   mapM_ ((>> threadDelay (600 * 1000)) . send conn . T.append "|/j ") Config.rooms

Send the server a list of commands in the form "|/w user, Harassed" where user is one of the users in the harass list. Wait 0.6 seconds after each command.

>   mapM_ ((>> threadDelay (600 * 1000)) . send conn . (flip T.append) ", Harassed." . T.append "|/w ") Config.harassList

Define a User and AssertionObject type for easy convertion from JSON.

> data User = User {
>   userid :: String,
>   usernum :: String,
>   username :: String,
>   email :: Maybe String,
>   registertime :: String,
>   group :: String,
>   banstate :: String,
>   ip :: String,
>   avatar :: String,
>   account :: Maybe String,
>   logintime :: String,
>   loginip :: String,
>   loggedin :: Bool,
>   outdatedpassword :: Bool
>   } deriving (Generic, Show)

> instance ToJSON User where
>   toEncoding = genericToEncoding defaultOptions

> instance FromJSON User

> data AssertionObject = AssertionObject {
>   curuser :: User,
>   actionsuccess :: Bool,
>   assertion :: String
>   } deriving (Generic, Show)

> instance ToJSON AssertionObject where
>   toEncoding = genericToEncoding defaultOptions

> instance FromJSON AssertionObject

Use the Aeson library to convert the JSON string into an AssertionObject and then simply extract the assertion from the Assertion Object.

> getAssertion :: String -> String
> getAssertion = assertion . either error id . eitherDecode . toLazyByteString
>   where toLazyByteString = B.fromStrict . encodeUtf8 . T.pack

The entry point to the program: it should connect to the Pokemon Showdown! Websocket and then run the bot.
Some terminals require the "setLocaleEncoding utf8" to handle some Unicode characters.

> main = do
>   setLocaleEncoding utf8
>   withSocketsDo $ WS.runClient Config.server Config.port Config.path app
