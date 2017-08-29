{-# LANGUAGE OverloadedStrings #-}
module Plugins.Mafia where

import           Control.Concurrent          (threadDelay)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Reader  (ReaderT, ask)
import           Data.IORef                  (modifyIORef, readIORef)
import           Data.List                   (nub)
import           System.Random               (newStdGen, randomRs)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import qualified Data.Text                   as T

import qualified Config
import qualified Hastebin
import           Plugins.Mafia.Types
import           Util

maybeRoomMafiaGame :: T.Text -> ReaderT Env IO () -> (MafiaGame -> ReaderT Env IO ()) -> ReaderT Env IO ()
maybeRoomMafiaGame room defaultValue f = do
  env <- ask
  maybeMafiaGame <- fmap (Map.lookup room) . liftIO . readIORef $ mafiaGames env
  maybe defaultValue f maybeMafiaGame

setRoomMafiaGame :: T.Text -> MafiaGame -> ReaderT Env IO ()
setRoomMafiaGame room game = do
  env <- ask
  liftIO . modifyIORef (mafiaGames env) $ Map.insert room game

deleteRoomMafiaGame :: T.Text -> ReaderT Env IO ()
deleteRoomMafiaGame room = do
  env <- ask
  liftIO . modifyIORef (mafiaGames env) $ Map.delete room

parseGameEffect :: T.Text -> GameEffect -> ReaderT Env IO ()
parseGameEffect room gameEffect =
  case getNewGame gameEffect of
    Just game -> do
      setRoomMafiaGame room game
      mapM_ (\message -> do
        say Config.mafiaColor room message
        liftIO . threadDelay $ 600 * 1000) $ getMessages gameEffect
    _         -> do
      mapM_ (\message -> do
        say Config.mafiaColor room message
        liftIO . threadDelay $ 600 * 1000) $ getMessages gameEffect
      onMafiaEndMessage room

onMafiaInitiateSS2Message :: T.Text -> ReaderT Env IO ()
onMafiaInitiateSS2Message = onMafiaInitiateMessageWithTheme ss2

onMafiaInitiateSS3Message :: T.Text -> ReaderT Env IO ()
onMafiaInitiateSS3Message = onMafiaInitiateMessageWithTheme ss3

onMafiaInitiateWnafMessage :: T.Text -> ReaderT Env IO ()
onMafiaInitiateWnafMessage = onMafiaInitiateMessageWithTheme wnaf

onMafiaInitiateMessageWithTheme :: Theme -> T.Text -> ReaderT Env IO ()
onMafiaInitiateMessageWithTheme gameTheme room =
  maybeRoomMafiaGame room startGame (const $ say Config.mafiaColor room "There is already a Mafia game going on!")
  where startGame = do
          say Config.mafiaColor room "**A new Mafia game is starting! Use** ``**@join**`` **to join!**"
          setRoomMafiaGame room $ OpenGame {players=Set.empty, theme=gameTheme}

onMafiaJoinMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onMafiaJoinMessage room user =
  maybeRoomMafiaGame room (return ()) $ (\mafiaGame ->
    operateOnMafiaGame mafiaGame (joinGame mafiaGame) $ pm Config.mafiaColor user "The Mafia game has already started.")
  where joinGame mafiaGame = do
          let mafiaGame' = mafiaGame {players=Set.insert (toId user) $ players mafiaGame}
          setRoomMafiaGame room mafiaGame'
          pm Config.mafiaColor user "You have joined the game of Mafia."
          if Set.size (players mafiaGame') == Set.findMax (validPlayerNumbers $ theme mafiaGame')
            then onMafiaStartMessage room
            else return ()

onMafiaLeaveMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onMafiaLeaveMessage room user =
  maybeRoomMafiaGame room (return ()) $ (\mafiaGame ->
    operateOnMafiaGame mafiaGame (leaveGame mafiaGame) $ pm Config.mafiaColor user "The Mafia game has already started.")
  where leaveGame mafiaGame = do
          let mafiaGame' = mafiaGame {players=Set.delete (toId user) $ players mafiaGame}
          setRoomMafiaGame room mafiaGame'
          pm Config.mafiaColor user "You have left the game of Mafia."

onMafiaAddPlayersMessage :: T.Text -> [T.Text] -> ReaderT Env IO ()
onMafiaAddPlayersMessage room users =
  maybeRoomMafiaGame room (return ()) $ (\mafiaGame ->
    operateOnMafiaGame mafiaGame (addToGame mafiaGame) $ say Config.mafiaColor room "The Mafia game has already started.")
  where addToGame mafiaGame = do
          let mafiaGame' = mafiaGame {players=Set.union (Set.map toId $ Set.fromList users) $ players mafiaGame}
          setRoomMafiaGame room mafiaGame'
          say Config.mafiaColor room $ "Following players added: " `T.append` T.intercalate ", " users

onMafiaKickPlayersMessage :: T.Text -> [T.Text] -> ReaderT Env IO ()
onMafiaKickPlayersMessage room users =
  maybeRoomMafiaGame room (return ()) $ (\mafiaGame ->
    operateOnMafiaGame mafiaGame (kickFromGame mafiaGame) $ say Config.mafiaColor room "The Mafia game has already started.")
  where kickFromGame mafiaGame = do
          let mafiaGame' = mafiaGame {players=Set.difference (players mafiaGame) $ Set.map toId (Set.fromList users)}
          setRoomMafiaGame room mafiaGame'
          say Config.mafiaColor room $ "Following players kicked: " `T.append` T.intercalate ", " users

onMafiaStartMessage :: T.Text -> ReaderT Env IO ()
onMafiaStartMessage room =
  maybeRoomMafiaGame room (return ()) $ (\mafiaGame ->
    operateOnMafiaGame mafiaGame (distributeRoles mafiaGame) $ say Config.mafiaColor room "The Mafia game has already started.")
  where gameSize mafiaGame = Set.size $ players mafiaGame
        distributeRoles mafiaGame =
          if gameSize mafiaGame `Set.member` validPlayerNumbers (theme mafiaGame)
            then distrib mafiaGame
            else say Config.mafiaColor room "You have not provided the right number of players for this theme."
        distrib mafiaGame = do
          let gameTheme = theme mafiaGame
          env <- ask
          say Config.mafiaColor room "Distributing roles..."
          randomRoles <- liftIO $ map (getRoleList gameTheme (Set.size $ players mafiaGame) !!) . nub . randomRs (0,length (getRoleList gameTheme (Set.size $ players mafiaGame))-1) <$> newStdGen
          let rolesMap   = Map.fromList $ zip (Set.toAscList $ players mafiaGame) randomRoles
              newGame    = ClosedGame {
                players=players mafiaGame,
                theme=gameTheme,
                playerRoles=rolesMap,
                day=1,
                gamePhase=Day,
                lynches=Map.fromList . zip (Set.toAscList $ players mafiaGame) $ repeat Nothing,
                miscInfo=[]}
              gameEffect = onStart gameTheme newGame
          Map.foldrWithKey (\user role acc -> do
            acc
            pm Config.mafiaColor user $ "Your role is: " `T.append` role
            liftIO . threadDelay $ 600 * 1000) (return ()) rolesMap
          case getNewGame gameEffect of
            Just game' -> do
              liftIO . modifyIORef (mafiaGames env) $ Map.insert room game'
              mapM_ (\message -> do
                say Config.mafiaColor room message
                liftIO . threadDelay $ 600 * 1000) $ getMessages gameEffect
              say Config.mafiaColor room "The Mafia game has started!"
            _          -> do
              mapM_ (\message -> do
                say Config.mafiaColor room message
                liftIO . threadDelay $ 600 * 1000) $ getMessages gameEffect
              onMafiaEndMessage room

onMafiaLynchMessage :: T.Text -> T.Text -> LynchVote -> ReaderT Env IO ()
onMafiaLynchMessage room user vote =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (return ()) $ lynchIn mafiaGame)
  where canLynchIn mafiaGame =
          toId user `Set.member` players mafiaGame &&
          fmap ((`Set.member` players mafiaGame) . toId) vote /= Just False &&
          not (vote == Nothing && not (allowedToNoLynch $ theme mafiaGame))
        lynchIn mafiaGame =
          if canLynchIn mafiaGame
            then do
              let (game', hammered) = placeLynchVote (toId user) (toId <$> vote) mafiaGame
              case hammered of
                Just voteCandidate ->
                  case voteCandidate of
                    Just player -> do
                      onMafiaEliminatePlayersMessage room [player]
                      maybeRoomMafiaGame room (return ()) (\mafiaGame'' ->
                        let gameEffect = onHammer (theme mafiaGame'') (toId user) (Just player) mafiaGame''
                        in parseGameEffect room gameEffect)
                    _           ->
                      let gameEffect = onHammer (theme mafiaGame) (toId user) Nothing game'
                      in parseGameEffect room gameEffect
                _         ->
                  let gameEffect = onLynch (theme mafiaGame) (toId user) vote game'
                  in parseGameEffect room gameEffect
            else return ()

onMafiaUnlynchMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onMafiaUnlynchMessage room user =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (return ()) $ unlynchIn mafiaGame)
  where unlynchIn mafiaGame =
          if toId user `Set.member` players mafiaGame
            then setRoomMafiaGame room $ mafiaGame {lynches=Map.insert (toId user) Nothing $ lynches mafiaGame}
            else return ()
      
onMafiaDisplayLynchesMessage :: T.Text -> ReaderT Env IO ()
onMafiaDisplayLynchesMessage room =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (return ()) $ displayLynchesIn mafiaGame)
  where displayPlayers users       = Set.foldr (\user acc ->
          if T.null acc
            then user
            else acc `T.append` ", " `T.append` user) "" users
        voteStr maybeVote =
          case maybeVote of
            Just vote ->
              case vote of
                Just user -> user
                _         -> "NL"
            _         -> "Und"
        votesForPlayer maybeVote users =
          voteStr maybeVote `T.append` " (" `T.append` (T.pack . show $ Set.size users) `T.append` "): " `T.append` displayPlayers users
        lynchesText mafiaGame      = Map.foldrWithKey (\maybeVote users acc ->
          if Set.null users
            then acc
            else
              if T.null acc
                then votesForPlayer maybeVote users
                else acc `T.append` "; " `T.append` votesForPlayer maybeVote users) "" $ invertMap mafiaGame (lynches mafiaGame)
        lynchesMessage mafiaGame = "**Majority: " `T.append` T.pack (show . majority . Set.size $ players mafiaGame) `T.append` "; " `T.append` lynchesText mafiaGame `T.append` "**"
        displayLynchesIn mafiaGame =
          say Config.mafiaColor room $ lynchesMessage mafiaGame

onMafiaTargetMessage :: T.Text -> T.Text -> [T.Text] -> ReaderT Env IO ()
onMafiaTargetMessage room user parts =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (return ()) $ parseTarget mafiaGame)
  where parseTarget mafiaGame =
          let gameEffect = onTarget (theme mafiaGame) (toId user) (toId <$> parts) mafiaGame
          in parseGameEffect room gameEffect


onMafiaEliminatePlayersMessage :: T.Text -> [T.Text] -> ReaderT Env IO ()
onMafiaEliminatePlayersMessage room users =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (say Config.mafiaColor room "The Mafia game has not started yet.") $ eliminateFromGame mafiaGame)
  where eliminateFromGame mafiaGame = do
          say Config.mafiaColor room $ "**Following players eliminated: " `T.append` foldr (\user acc ->
            if T.null acc
              then case Map.lookup user $ playerRoles mafiaGame of
                Just role -> user `T.append` " the " `T.append` role
                _         -> user `T.append` " is not in the game"
              else case Map.lookup user $ playerRoles mafiaGame of
                Just role -> acc `T.append` "; " `T.append` user `T.append` " the " `T.append` role
                _         -> acc `T.append` "; " `T.append` user `T.append` " is not in the game") "" (map toId users) `T.append` "**"
          setRoomMafiaGame room $ mafiaGame
            {players=Set.difference (players mafiaGame) $ Set.map toId (Set.fromList users),
             lynches=Map.fromList . zip (Set.toAscList $ players mafiaGame) $ repeat Nothing}

onMafiaDisplayPlayersMessage :: T.Text -> ReaderT Env IO ()
onMafiaDisplayPlayersMessage room =
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (return ()) $ displayPlayersInGame mafiaGame)
  where gameSize mafiaGame = T.pack . show . Set.size $ players mafiaGame
        playersStr mafiaGame = T.intercalate ", " . Set.toAscList $ players mafiaGame
        playersInGameStr mafiaGame = "Players (" `T.append` gameSize mafiaGame `T.append` "): " `T.append` playersStr mafiaGame
        displayPlayersInGame mafiaGame = say Config.mafiaColor room $ "**" `T.append` T.strip (playersInGameStr mafiaGame) `T.append` "**"

onMafiaEndMessage :: T.Text -> ReaderT Env IO ()
onMafiaEndMessage room = do
  maybeRoomMafiaGame room (return ()) (\mafiaGame ->
    operateOnMafiaGame mafiaGame (say Config.mafiaColor room "Mafia game ended.") $ hastebinRolesInGame mafiaGame)
  deleteRoomMafiaGame room
  where hastebinRolesInGame mafiaGame = do
          url <- Hastebin.upload . Map.foldrWithKey (\user role acc ->
            if T.null acc
              then user `T.append` ": " `T.append` role
              else acc `T.append` ";\n" `T.append` user `T.append` ": " `T.append` role) "" $ playerRoles mafiaGame
          say Config.mafiaColor room $ "Mafia game ended. Roles: " `T.append` url