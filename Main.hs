{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Main where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM.TMChan (TMChan, newTMChanIO)
import           Control.Concurrent.MVar       (MVar, newMVar)
import           Network.HTTP                  (simpleHTTP, postRequestWithBody, getResponseBody)
import           Network.HTTP.Base             (urlEncode, rspReason)
import           Network.Socket                (withSocketsDo)
import           Control.Monad                 (forever, unless)
import           Control.Monad.Trans           (liftIO)
import           Data.Aeson                    (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Data.List                     (isPrefixOf, isInfixOf, group, maximumBy, sort)
import           Data.Ord                      (comparing)
import           Data.Text                     (Text)
import           GHC.Generics                  (Generic)
import           GHC.IO.Encoding               (setLocaleEncoding, utf8)
import           Data.Text.Encoding            (encodeUtf8)
import           System.Random                 (Random, random, randomR, randomRIO, randomIO)
import qualified System.IO.Strict              as S
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Network.WebSockets            as WS

import qualified Config
import           Util

app :: WS.ClientApp ()
app conn = do
  lock <- newMVar ()
  chanLock <- newMVar ()
  logText lock Config.connectedColor . T.pack $ "Successfully connected to " ++ "ws://" ++ Config.server ++ ":" ++ (show Config.port) ++ Config.path
  chan <- newTMChanIO
  _ <- forkIO $ forever $ WS.receiveData conn >>= parse lock chanLock conn chan
  let loop = do
        line <- T.getLine
        unless (T.null line) $ sendWithColor lock conn Config.logColor line >> loop
  loop
  WS.sendClose conn ("|/logout" :: Text)
  threadDelay $ 1000 * 1000

data UNOColor = UNOWild | UNOBlue | UNOGreen | UNORed | UNOYellow deriving (Eq, Ord)

readUNOColor :: T.Text -> UNOColor
readUNOColor "Wild" = UNOWild
readUNOColor "Blue" = UNOBlue
readUNOColor "Green" = UNOGreen
readUNOColor "Red" = UNORed
readUNOColor "Yellow" = UNOYellow
readUNOColor str = error . T.unpack $ "No such UNO color: " `T.append` str

colorToText :: UNOColor -> T.Text
colorToText UNOWild = "Wild"
colorToText UNOBlue = "Blue"
colorToText UNOGreen = "Green"
colorToText UNORed = "Red"
colorToText UNOYellow = "Yellow"

rgbToUNOColor :: T.Text -> UNOColor
rgbToUNOColor "rgb(0, 128, 0)" = UNOGreen
rgbToUNOColor "rgb(175, 165, 40)" = UNOYellow
rgbToUNOColor "rgb(75, 75, 255)" = UNOBlue
rgbToUNOColor "rgb(255, 0, 0)" = UNORed
rgbToUNOColor "inherit" = UNOWild
rgbToUNOColor color = error . T.unpack $ "rgbToUNOColor: " `T.append` color `T.append` " is not a valid UNO color."

data UNONumber = None | Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine | Reverse | Skip | PlusTwo | PlusFour deriving (Eq)

readUNONumber :: T.Text -> UNONumber
readUNONumber "" = None
readUNONumber "0" = Zero
readUNONumber "1" = One
readUNONumber "2" = Two
readUNONumber "3" = Three
readUNONumber "4" = Four
readUNONumber "5" = Five
readUNONumber "6" = Six
readUNONumber "7" = Seven
readUNONumber "8" = Eight
readUNONumber "9" = Nine
readUNONumber "Reverse" = Reverse
readUNONumber "Skip" = Skip
readUNONumber "+2" = PlusTwo
readUNONumber "+4" = PlusFour
readUNONumber str = error . T.unpack $ "No such UNO number: " `T.append` str

numberToText :: UNONumber -> T.Text
numberToText None = ""
numberToText Zero = "0"
numberToText One = "1"
numberToText Two = "2"
numberToText Three = "3"
numberToText Four = "4"
numberToText Five = "5"
numberToText Six = "6"
numberToText Seven = "7"
numberToText Eight = "8"
numberToText Nine = "9"
numberToText Reverse = "Reverse"
numberToText Skip = "Skip"
numberToText PlusTwo = "+2"
numberToText PlusFour = "+4"

data UNOCard = UNOCard {color :: UNOColor, number :: UNONumber}

cardToText :: UNOCard -> T.Text
cardToText (UNOCard {color = c, number = None}) = colorToText c
cardToText (UNOCard {color = c, number = n}) = colorToText c `T.append` " " `T.append` numberToText n

readUNOCard :: T.Text -> UNOCard
readUNOCard str = UNOCard {color = cardColor, number = cardNumber}
  where parts = T.splitOn " " str
        cardColor = readUNOColor $ head parts
        cardNumber = if null $ tail parts then None else readUNONumber . head $ tail parts

parse :: MVar () -> MVar () -> WS.Connection -> TMChan T.Text -> T.Text -> IO ()
parse lock chanLock conn chan s = logText lock Config.logColor s >> parse' conn s
  where parse' conn s
          | T.head s == '>' =
            if null$tail messageList
              then return ()
              else
                let room = T.init.T.tail$messageList !! 0
                in case head$tail messageList of
                  "c"     -> if (messageList !! 2) == "~"
                    then
                      if (messageList !! 4) == T.pack Config.username `T.append` "'s turn."
                        then basedOnChan_ lock chanLock chan Config.unoColor (\_ -> logChan lock chan Config.unoColor "UNO: My turn")
                        else return ()
                    else commandParse lock conn room (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  "c:"    -> if (messageList !! 3) == "~"
                    then
                      if (messageList !! 4) == T.pack Config.username `T.append` "'s turn."
                        then basedOnChan_ lock chanLock chan Config.unoColor (\_ -> logChan lock chan Config.unoColor "UNO: My turn")
                        else return ()
                    else commandParse lock conn room (messageList !! 3) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
                  "raw"   -> if "You have drawn the following card: " `T.isPrefixOf` (messageList !! 2)
                    then basedOnChan_ lock chanLock chan Config.unoColor (\_ ->
                      let card = T.tail . fst . T.breakOn "<" . snd . T.breakOn ">" $ messageList !! 2
                      in  logChan lock chan Config.unoColor $ "UNO: Drew " `T.append` card)
                    else return ()
                  "uhtml" -> processHtmlResponse lock chanLock conn chan room (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  "uhtmlchange" -> processHtmlChange lock chanLock conn chan room (messageList !! 2) $ T.intercalate "|" (tail.tail.tail$messageList)
                  _       -> return ()
          | T.head s /= '|' = return ()
          | "|challstr" `T.isPrefixOf` s =
            let postData = "act=login&name=" ++ urlEncode Config.username ++ "&pass=" ++ urlEncode Config.password ++ "&challstr=" ++ urlEncode (T.unpack $ T.drop 10 s)
            in  do
              httpResponse <- simpleHTTP (postRequestWithBody Config.actionUrl "application/x-www-form-urlencoded" postData)
              (_:jsonData) <- getResponseBody httpResponse
              let loginCommand = "/trn " ++ Config.username ++ ",0," ++ getAssertion jsonData
              useCommand lock conn Config.loginColor $ T.pack loginCommand
              startUpActions lock conn
          | "|pm" `T.isPrefixOf` s = commandParse lock conn "pm" (messageList !! 2) $ T.intercalate "|" (tail.tail.tail.tail$messageList)
          | otherwise = return ()
            where messageList = T.splitOn "|" s
        processHtmlResponse lock chanLock conn chan room messageType message
          | "hangman" `T.isPrefixOf` messageType = do
            forkIO $ mapM_ ((>> threadDelay (600 * 1000)) . say lock conn Config.hangmanColor room . ("/hangman guess " `T.append`)) ["o","d","m"]
            return ()
          | "uno-hand" == messageType = do
            forkIO $ do
              let cards = map (readUNOCard . T.drop (T.length "/uno play ")) . filter ("/uno play " `T.isPrefixOf`) $ T.splitOn "\"" message
              if null cards
                then return ()
                else
                  let process lock chan chanMessage
                        | "UNO hand display change" == chanMessage = do
                          if length cards == 1
                            then do
                              let unoCommand = head . filter ("/uno uno " `T.isPrefixOf`) $ T.splitOn "\"" message
                              say lock conn Config.unoColor room unoCommand
                              logChan lock chan Config.unoColor "UNO: Saying UNO"
                            else logChan lock chan Config.unoColor "UNO hand display change acknowledged"
                        | "UNO: Drew" `T.isPrefixOf` chanMessage =
                          logChan lock chan Config.unoColor chanMessage
                        | otherwise = do
                            logChan lock chan Config.unoColor "UNO: Making my decision"
                            logText lock Config.unoColor . ("UNO CARDS: " `T.append`) . T.pack . show $ map cardToText cards
                            let topCard = readUNOCard (fst $ T.breakOn "<" (T.tail . snd $ T.breakOn ">" (snd $ T.breakOn "Top Card: " message)))
                            let topCardAccountingWild = if color topCard == UNOWild
                                then
                                  let rgb = T.takeWhile (/= '\"') . T.drop (T.length "Top Card: <span style=\"color: ") . snd $ T.breakOn "Top Card: <span style=\"color: " message
                                  in  UNOCard {color = rgbToUNOColor rgb, number = None}
                                else topCard
                            let wildCards = filter ((== UNOWild) . color) cards
                            let nonWildCards = filter ((/= UNOWild) . color) cards
                            let nonWildPlayableCards = filter (\card -> color card == color topCardAccountingWild || number card == number topCardAccountingWild) nonWildCards
                            if null nonWildPlayableCards
                              then
                                if null wildCards
                                  then do
                                    say lock conn Config.unoColor room "/uno draw"
                                    let loop = do
                                          threadDelay $ 500 * 1000
                                          logText lock Config.unoColor "Loop"
                                          basedOnChan lock chanLock chan Config.unoColor (\chanMessage ->
                                            case chanMessage of
                                              Just message -> if "UNO: Drew " `T.isPrefixOf` message
                                                then return $ T.drop (T.length "UNO: Drew ") message
                                                else do
                                                  logChan lock chan Config.unoColor message
                                                  loop
                                              _            -> return "")
                                    drawnCardText <- loop
                                    let drawnCard = readUNOCard drawnCardText
                                    if color drawnCard == UNOWild
                                      then do
                                        let modeColor = if null nonWildCards
                                              then UNOBlue
                                              else head . maximumBy (comparing length) . group . sort $ map color nonWildCards
                                        say lock conn Config.unoColor room $ "/uno play " `T.append` cardToText drawnCard
                                        threadDelay $ 600 * 1000
                                        say lock conn Config.unoColor room $ "/uno color " `T.append` colorToText modeColor
                                      else
                                        if color drawnCard == color topCardAccountingWild || number drawnCard == number topCardAccountingWild
                                          then say lock conn Config.unoColor room $ "/uno play " `T.append` cardToText drawnCard
                                          else say lock conn Config.unoColor room "/uno pass"
                                  else do
                                    let modeColor = if null nonWildCards
                                          then UNOBlue
                                          else head . maximumBy (comparing length) . group . sort $ map color nonWildCards
                                    say lock conn Config.unoColor room $ "/uno play " `T.append` cardToText (head wildCards)
                                    threadDelay $ 600 * 1000
                                    say lock conn Config.unoColor room $ "/uno color " `T.append` colorToText modeColor
                              else say lock conn Config.unoColor room $ "/uno play " `T.append` cardToText (head nonWildPlayableCards)
                  in  readFromChan lock chan Config.unoColor >>= (\chanMessage ->
                        case chanMessage of 
                          Just message -> process lock chan message
                          _            -> return ())
            return ()
          | "uno" `T.isPrefixOf` messageType = do
            say lock conn Config.unoColor room "/uno join"
            logChan lock chan Config.unoColor "UNO: Joined game"
          | otherwise = return ()
        processHtmlChange :: MVar () -> MVar () -> WS.Connection -> TMChan T.Text -> T.Text -> T.Text -> T.Text -> IO ()
        processHtmlChange lock chanLock conn chan room messageType message
          | "uno-hand" == messageType =
            let process chanMessage
                  | "UNO: My turn" == chanMessage           =
                      logChan lock chan Config.unoColor chanMessage
                  | "UNO: Drew " `T.isPrefixOf` chanMessage =
                      logChan lock chan Config.unoColor chanMessage
                  | otherwise                               =
                      logChan lock chan Config.unoColor "UNO hand display change"
                changeMessage (Just message) = process message
                changeMessage _              = return ()
            in basedOnChan_ lock chanLock chan Config.unoColor changeMessage
          | "uno" `T.isPrefixOf` messageType =
            if "The game of UNO has ended." `T.isInfixOf` message
              then basedOnChan_ lock chanLock chan Config.unoColor (\_ -> return ())
              else return ()
          | otherwise = return ()

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

checkSetting :: String -> (Maybe String -> Bool) -> IO () -> IO () -> IO ()
checkSetting setting predicate thenAction elseAction = do
  setting' <- getSetting setting
  if predicate setting'
    then thenAction
    else elseAction

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

commandParse :: MVar () -> WS.Connection -> T.Text -> T.Text -> T.Text -> IO ()
commandParse lock conn room username message
  | "should " `T.isPrefixOf` (T.toLower message) =
    checkSetting "should auto-reply no" (==Just "True")
      (if room == "pm"
        then pm lock conn Config.commandColor username "No."
        else say lock conn Config.commandColor room "No.") $ return ()
  | "stop saying no" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should auto-reply no" "False"
  | "start saying no" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should auto-reply no" "True"
  | "say " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    sendWithColor lock conn Config.commandColor (T.tail.T.tail.T.tail.T.tail$message)
  | "github please" `T.isInfixOf` (T.toLower message) =
    pm lock conn Config.commandColor username "https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"
  | "what is java" `T.isPrefixOf` (T.toLower message) = do
    forkIO $ do
      pm lock conn Config.commandColor username "Java is a general-purpose (aka vaguely defined purpose) computer programming language (voodoo) that is concurrent (bad at multi-tasking), class-based (tries to be well structured), object-oriented (cannot process abstract ideas), and specifically designed to have as few implementation dependencies"
      pm lock conn Config.commandColor username "as possible (isolated from reality)."
      threadDelay $ 1*1000*1000
      pm lock conn Config.commandColor username "It is intended to let application developers \"write once, run anywhere\", meaning that compiled Java code can run on all platforms that support Java without the need for recompilation"
      pm lock conn Config.commandColor username "(making the arms of corporate conspiracies to quietly take over the modern society stretch all that farther)."
    return ()
  | "ab" == message =
    pm lock conn Config.commandColor username $ "Identity confirmed: " `T.append` T.tail username
  | "in this room, " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
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
  | "no more room commands" `T.isPrefixOf` (T.toLower message) =
    writeFile "roomcommands.txt" ""
  | (toId (T.pack Config.username) `T.append` "choose") `T.isPrefixOf` (toId message) =
    checkSetting "should choose" (==Just "True") (do
      choice <- randomIO :: IO Choice
      let textChoice = T.pack $ show choice
      if room == "pm"
        then pm lock conn Config.commandColor username textChoice
        else say lock conn Config.commandColor room textChoice) $ return ()
  | "stop choosing" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should choose" "False"
  | "start choosing" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should choose" "True"
  | "vinegar garbodor with an aside of trubbish" == message =
    pm lock conn Config.commandColor username "vinegar trubbish with an aside of garbodor"
  | "can i" `T.isInfixOf` (T.toLower message) || "can we" `T.isInfixOf` (T.toLower message) = 
    checkSetting "should can I auto-reply" (==Just "True")
      (if room == "pm"
        then pm lock conn Config.commandColor username "No, you cannot."
        else say lock conn Config.commandColor room "No, you cannot.") $ return ()
  | "start telling people what they can do" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should can I auto-reply" "True"
  | "stop telling people what they can do" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    setSetting "should can I auto-reply" "False"
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
              pm lock conn Config.commandColor username $ T.replicate n "?"
        return ()
      else do
        commands <- S.readFile "roomcommands.txt"
        let searchedCommand = filter ((T.unpack room ++ " -> " ++ T.unpack message ++ " -> ")`isPrefixOf`) $ lines commands
        if null searchedCommand
          then return ()
          else say lock conn Config.commandColor room . head.tail.tail . T.splitOn " -> " . T.pack $ head searchedCommand

startUpActions :: MVar () -> WS.Connection -> IO ()
startUpActions lock conn = do
  useCommand lock conn Config.startUpColor "/avatar 120"
  threadDelay $ 600 * 1000
  mapM_ ((>> threadDelay (600 * 1000)) . useCommand lock conn Config.startUpColor  . ("/j " `T.append`)) Config.rooms
  mapM_ ((>> threadDelay (600 * 1000)) . ($ "Harassed.") . pm lock conn Config.startUpColor) Config.harassList

data AssertionObject = AssertionObject {assertion :: String} deriving (Generic, Show)

instance ToJSON AssertionObject where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AssertionObject

getAssertion :: String -> String
getAssertion = assertion . either error id . eitherDecode . toLazyByteString
  where toLazyByteString = B.fromStrict . encodeUtf8 . T.pack

main = do
  setLocaleEncoding utf8
  withSocketsDo $ WS.runClient Config.server Config.port Config.path app
