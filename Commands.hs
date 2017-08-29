{-# LANGUAGE OverloadedStrings #-}
module Commands where

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Monad                 ((<=<))
import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT, ask, runReaderT)
import           Data.List                     (intersperse, isPrefixOf, maximumBy)
import           Data.Ord                      (comparing)
import           System.Random                 (Random, random, randomR, randomRIO, randomIO)
import qualified Data.Text                     as T
import qualified System.IO.Strict              as S

import           Commands.Permissions
import           Commands.Settings
import qualified Config
import qualified Plugins.Chess                 as Chess
import qualified Plugins.Mafia                 as Mafia
import qualified Plugins.GreatestIdea          as Gesti
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
    liftIO $ checkSetting "should auto-reply no" (T.unpack room) (==Just "True") (
      if room == "pm"
        then runReaderT (pm Config.commandColor username "No.") env
        else runReaderT (say Config.commandColor room "No.") env) $ return ()
  | "stop saying no in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should auto-reply no" (T.unpack . T.drop (T.length "stop saying no in ") . snd . T.breakOn "stop saying no in " $ T.toLower message) "False"
  | "start saying no in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should auto-reply no" (T.unpack . T.drop (T.length "start saying no in ") . snd . T.breakOn "start saying no in " $ T.toLower message) "True"
  | "sayraw " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let unEscape = read . T.unpack . ("\"" `T.append`) . (`T.append` "\"") . T.replace "\"" "\\\""
    in sendWithColor Config.commandColor . unEscape $ T.drop (T.length "sawraw ") message
  | "say " `T.isPrefixOf` (T.toLower message) = do
    env <- ask
    liftIO $ checkRankText username (>=5) (
      let room'           = T.drop (T.length "say ") . fst $ T.breakOn "," message
          escapeCommand t = if T.head t == '/' then T.cons '/' t else t
          message'        = T.tail . snd $ T.breakOn "," message
      in
        if length (T.splitOn "," message) > 1
          then runReaderT (say Config.commandColor room' $ escapeCommand message') env
          else return ()) $ return ()
  | "github please" `T.isInfixOf` (T.toLower message) =
    pm Config.commandColor username "https://github.com/JavaIsTheWorst/Pokemon-Showdown-Bot"
  | "hi " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    pm Config.commandColor username "hi"
  | "what is java" `T.isPrefixOf` (T.toLower message) = do
    env <- ask
    _ <- liftIO . forkIO $ do
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
    in if length parts < 3
         then return ()
         else liftIO $ do
           commands <- S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack . T.intercalate " -> " $ room : (init parts))`isPrefixOf`)) $ lines commands
               updatedCommands     = unlines $ (T.unpack . T.intercalate " -> " $ room : parts) : filteredCommandList
           writeFile "roomcommands.txt" updatedCommands
  | "in this room, establish pm system: " `T.isPrefixOf` (T.toLower message) =
    liftIO $ checkRankText username (>=5) (
      let parts     = T.splitOn " -> " $ T.drop (T.length "in this room, establish PM system: ") message
      in if length parts < 3
           then return ()
           else do
             commands <- S.readFile "roomcommands.txt"
             let escapeCommand t = if T.head t == '/' then T.cons '/' t else t
                 partsSafe =
                   case splitAt 2 parts of
                     (xs,y:ys) -> xs ++ ("|/w $(user)," `T.append` escapeCommand y) : ys
                     (xs,_)    -> xs
                 filteredCommandList = filter (not . ((T.unpack . T.intercalate " -> " $ room : (init parts))`isPrefixOf`)) $ lines commands
                 updatedCommands     = unlines $ (T.unpack . T.intercalate " -> " $ room : partsSafe) : filteredCommandList
             writeFile "roomcommands.txt" updatedCommands) $ return ()
  | "delete relation " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let parts = T.splitOn " -> " $ T.drop (T.length "delete relation ") message
    in if length parts < 3
         then return ()
         else do
           commands <- liftIO $ S.readFile "roomcommands.txt"
           let filteredCommandList = filter (not . ((T.unpack . T.intercalate " -> " $ room : parts)==)) $ lines commands
           liftIO . writeFile "roomcommands.txt" $ unlines filteredCommandList
  | "choose" `T.isPrefixOf` (toId message) = do
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
    liftIO $ checkSetting "should can I auto-reply" (T.unpack room) (==Just "True") (
      if room == "pm"
        then runReaderT (pm Config.commandColor username "No, you cannot.") env
        else runReaderT (say Config.commandColor room "No, you cannot.") env) $ return ()
  | "start telling people what they can do in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should can I auto-reply" (T.unpack . T.drop (T.length "start telling people what they can do in ") . snd . T.breakOn "start telling people what they can do in " $ T.toLower message) "True"
  | "stop telling people what they can do in " `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    liftIO $ setSetting "should can I auto-reply" (T.unpack . T.drop (T.length "stop telling people what they can do in ") . snd . T.breakOn "stop telling people what they can do in " $ T.toLower message) "False"
  | "initiate chess: " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let players = T.breakOn " and " . T.drop (T.length "initiate chess: ") $ message
        playerOne = toId . fst $ players
        playerTwo = T.drop (T.length "and") . toId . snd $ players
    in
      if playerOne == "" || playerTwo == ""
        then return ()
        else Chess.onInitiateMessage playerOne playerTwo
  | "move: " `T.isPrefixOf` (T.toLower message) =
    let move = T.drop (T.length "move: ") message
    in Chess.onChessMoveMessage (toId username) move
  | "import fen: " `T.isPrefixOf` (T.toLower message) =
    let fen = T.drop (T.length "import fen: ") message
    in Chess.onImportFENMessage (toId username) fen
  | "import moves: " `T.isPrefixOf` (T.toLower message) =
    let moves = T.drop (T.length "import moves: ") message
    in Chess.onImportMovesMessage (toId username) moves
  | "end current chess host" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Chess.onChessEndMessage
  | "initiate gesti: " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let (host:players) = T.splitOn "," $ T.drop (T.length "initiate gesti: ") message
    in Gesti.onInitiateMessage (toId host) (map toId players)
  | "gestipick: " `T.isPrefixOf` (T.toLower message) =
    let (alignmentNumber:roleNumber:_) = (T.splitOn "," $ T.drop (T.length "gestipick: ") message) ++ [""]
    in Gesti.onPickMessage (toId username) (T.strip alignmentNumber) (T.strip roleNumber)
  | "end gesti choosing period" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Gesti.onGestiEndMessage
  | "give the permission " `T.isInfixOf` (T.toLower message) = do
    env <- ask
    let user       = T.drop (T.length " to ") . snd $ T.breakOn " to " message
        permission = read . T.unpack . T.drop (T.length "give the permission ") . snd . T.breakOn "give the permission " . fst $ T.breakOn " to " message
    liftIO $ checkRankText username (> abs (permission*2) + 1) (
      if permission == 0
        then do
          runReaderT (if room == "pm"
            then pm Config.commandColor username $ user `T.append` " has been demoted."
            else say Config.commandColor room $ user `T.append` " has been demoted.") env
          demoteText user
        else do
          runReaderT (if room == "pm"
            then pm Config.commandColor username $ user `T.append` " has been given rank " `T.append` T.pack (show permission)
            else say Config.commandColor room $ user `T.append` " has been given rank " `T.append` T.pack (show permission)) env
          setRankText user permission) $ return ()
  | "check the permissions of " `T.isInfixOf` (T.toLower message) = do
    env <- ask
    liftIO $ checkRankText username (>=0) (runReaderT (
      let user = T.drop (T.length "check the permissions of ") . snd $ T.breakOn "check the permissions of " message
      in pm Config.commandColor username =<< ("Permission of " `T.append` user `T.append` ": " `T.append`) . T.pack . show <$> liftIO (getRankText user)) env) $ return ()
  | "initiate ss2" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaInitiateSS2Message room
  | "initiate ss3" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaInitiateSS3Message room
  | "initiate wnaf" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaInitiateWnafMessage room
  | "@j" `T.isPrefixOf` (T.toLower message) || "@join" `T.isPrefixOf` (T.toLower message) =
    Mafia.onMafiaJoinMessage room username
  | "@leave" `T.isPrefixOf` (T.toLower message) =
    Mafia.onMafiaLeaveMessage room username
  | "@add " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) = 
    let users = T.splitOn "," $ T.drop (T.length "@add ") message
    in Mafia.onMafiaAddPlayersMessage room users
  | "@kick " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let users = T.splitOn "," $ T.drop (T.length "@kick ") message
    in Mafia.onMafiaKickPlayersMessage room users
  | "@start" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaStartMessage room
  | "@l " `T.isPrefixOf` (T.toLower message) =
    let vote = T.drop (T.length "@l ") message
    in Mafia.onMafiaLynchMessage room username $ Just vote
  | "@nl" `T.isPrefixOf` (T.toLower message) =
    Mafia.onMafiaLynchMessage room username Nothing
  | "@ul" `T.isPrefixOf` (T.toLower message) =
    Mafia.onMafiaUnlynchMessage room username
  | "@0" `T.isPrefixOf` (T.toLower message) =
    Mafia.onMafiaDisplayLynchesMessage room
  | "@target " `T.isPrefixOf` (T.toLower message) =
    case T.splitOn "," $ T.drop (T.length "@target ") message of
      (targetRoom:parts) -> Mafia.onMafiaTargetMessage targetRoom username parts
      _                  -> return ()
  | "@kill " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    let users = T.splitOn "," $ T.drop (T.length "@kill ") message
    in Mafia.onMafiaEliminatePlayersMessage room users
  | "@pl" `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaDisplayPlayersMessage room
  | "@end" `T.isInfixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    Mafia.onMafiaEndMessage room
  | "/invite " `T.isPrefixOf` (T.toLower message) && toId username == toId (T.pack Config.owner) =
    if room == "pm"
      then
        let roomToJoin = T.drop (T.length "/invite ") message
        in useGlobalCommand Config.commandColor $ "/j " `T.append` roomToJoin
      else return ()
  | otherwise = do
    env <- ask
    liftIO $ checkRankText username (>=0) (runReaderT (
      if room == "pm" && toId username /= toId (T.pack Config.username) && Config.respondWithQuestionMarks
        then do
          _ <- liftIO . forkIO $ do
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
          rank <- liftIO $ getRank $ T.unpack username
          let searchedCommand  = filter (\line ->
                let parts = T.splitOn " -> " $ T.pack line
                in ((parts !! 0) == room || (parts !! 0) == "all") && (parts !! 1) == message && read (T.unpack $ parts !! 2) <= rank) $ lines commands
          if null searchedCommand
            then return ()
            else
              let unEscape           = read . T.unpack . ("\"" `T.append`) . (`T.append` "\"") . T.replace "\"" "\\\""
                  splitText          = T.splitOn " $(pick) "
                  processRandoms s   =
                    let choices = splitText s
                    in if length choices == 1
                      then return s
                      else (choices !!) <$> (liftIO $ randomRIO (0,length choices - 1))
                  getResponse        = last . T.splitOn " -> " . T.pack
                  getPermission :: String -> Integer
                  getPermission      = read . T.unpack . (!!2) . T.splitOn " -> " . T.pack
                  response           = getResponse $ maximumBy (comparing getPermission) searchedCommand
              in sendWithColor Config.commandColor <=< processRandoms . T.replace "$(user)" username . unEscape $ response) env) $ return ()

startUpActions :: ReaderT Env IO ()
startUpActions = do
  env <- ask
  _ <- liftIO . forkIO $ do
    runReaderT (useGlobalCommand Config.startUpColor "/avatar 120") env
    threadDelay $ 600 * 1000
    let joinsReaderT   = map (useGlobalCommand Config.startUpColor . ("/j " `T.append`)) Config.rooms
        joins          = map (($ env) . runReaderT) joinsReaderT
        joinsWithDelay = intersperse (threadDelay $ 600 * 1000) joins
    sequence_ joinsWithDelay
    threadDelay $ 600 * 1000
    let pmsReaderT     = map (($ "Harassed.") . pm Config.startUpColor) Config.harassList
        pms            = map (($ env) . runReaderT) pmsReaderT
        pmsWithDelay   = intersperse (threadDelay $ 600 * 1000) pms
    sequence_ pmsWithDelay
  return ()