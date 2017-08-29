{-# LANGUAGE OverloadedStrings #-}
module Plugins.Mafia.Types where

import           Data.Maybe (fromJust, isJust)
import qualified Data.Map   as Map
import qualified Data.Set   as Set
import qualified Data.Text  as T

type LynchVote = Maybe T.Text

type Role = T.Text

data GamePhase = Day | Twilight | Night deriving (Show)

data MafiaGame =
  OpenGame {
    players :: Set.Set T.Text,
    theme :: Theme} |
  ClosedGame {
    players :: Set.Set T.Text,
    theme :: Theme,
    playerRoles :: Map.Map T.Text Role,
    day :: Int,
    gamePhase :: GamePhase,
    lynches :: Map.Map T.Text (Maybe LynchVote),
    miscInfo :: [T.Text]} deriving (Show)

operateOnMafiaGame :: MafiaGame -> a -> a -> a
operateOnMafiaGame game onOpen onClosed =
  case game of
    OpenGame {}   -> onOpen
    ClosedGame {} -> onClosed

voteOptions :: MafiaGame -> Set.Set (Maybe LynchVote)
voteOptions = Set.insert Nothing . Set.map Just . Set.insert Nothing . Set.map Just . players

invertMap :: MafiaGame -> Map.Map T.Text (Maybe LynchVote) -> Map.Map (Maybe LynchVote) (Set.Set T.Text)
invertMap game =
  Map.foldrWithKey (\user maybeVote acc ->
    Map.adjust (Set.insert user) maybeVote acc) $ Map.fromList (zip (Set.toAscList $ voteOptions game) $ repeat Set.empty)

majority :: (Integral a) => a -> a
majority n = (n `div` 2) + 1

placeLynchVote :: T.Text -> LynchVote -> MafiaGame -> (MafiaGame, Maybe LynchVote)
placeLynchVote user vote game@(ClosedGame {}) =
  let checkForHammer lynches'      =
        let (possibleVote, reachedMaj) = Map.foldrWithKey (\maybeVote users (maybeVoteAcc, reachedMajAcc) ->
              if reachedMajAcc
                then (maybeVoteAcc, reachedMajAcc)
                else (maybeVote, Set.size users >= majority (Set.size $ players game))) (Nothing, False) $ invertMap game lynches'
        in
          if reachedMaj && isJust possibleVote
            then case fromJust possibleVote of
              Just votedUser -> (game, Just $ Just votedUser)
              _              -> (game, Just Nothing)
            else (game {lynches=lynches'}, Nothing)
  in checkForHammer . Map.insert user (Just vote) $ lynches game
placeLynchVote _ _ game = (game, Nothing)

data GameEffect = GameEffect {getMessages :: [T.Text], getNewGame :: Maybe MafiaGame}

mkGameEffect :: [T.Text] -> Maybe MafiaGame -> GameEffect
mkGameEffect messages game = GameEffect {getMessages=messages, getNewGame=game}

messagesEffect :: [T.Text] -> MafiaGame -> GameEffect
messagesEffect messages game = mkGameEffect messages $ Just game

endGameEffect :: [T.Text] -> GameEffect
endGameEffect messages = mkGameEffect messages Nothing

nullGameEffect :: MafiaGame -> GameEffect
nullGameEffect game = mkGameEffect [] $ Just game

data Theme = Theme {
  getRoleList :: Int -> [Role],
  validPlayerNumbers :: Set.Set Int,
  allowedToNoLynch :: Bool,
  onStart :: MafiaGame -> GameEffect,
  onLynch :: T.Text -> LynchVote -> MafiaGame -> GameEffect,
  onHammer :: T.Text -> LynchVote -> MafiaGame -> GameEffect,
  onTarget :: T.Text -> [T.Text] -> MafiaGame -> GameEffect}

instance Show Theme where show = const ""

defaultTheme :: Theme
defaultTheme = Theme {
  getRoleList= const [],
  validPlayerNumbers = Set.empty,
  allowedToNoLynch = True,
  onStart = messagesEffect ["Day 1 Start!"],
  onLynch = const $ const nullGameEffect,
  onHammer = const $ const nullGameEffect,
  onTarget = const $ const nullGameEffect}

toRoleListFunction :: [(Int, [Role])] -> Int -> [Role]
toRoleListFunction assocList n =
  maybe [] id $ lookup n assocList

ss2 :: Theme
ss2 = defaultTheme {
  getRoleList = toRoleListFunction [(2, ["Hated Mafia", "Hated Supersaint", "Hated Supersaint"])],
  validPlayerNumbers = Set.singleton 2,
  onLynch = \_ target game ->
    case target of
      Just _ ->
        case Map.elems $ playerRoles game of
          ["Hated Supersaint", "Hated Supersaint"] -> mkGameEffect ["No one has won!"] Nothing
          _                                        -> mkGameEffect ["The Town has won!"] Nothing
      _         -> nullGameEffect game,
  onHammer = \_ target game ->
    case target of
      Just _ -> nullGameEffect game
      _      ->
        case Map.elems $ playerRoles game of
          ["Hated Supersaint", "Hated Supersaint"] -> mkGameEffect ["The Town has won!"] Nothing
          _                                        -> mkGameEffect ["The Mafia has won!"] Nothing}

ss3 :: Theme
ss3 = defaultTheme {
  getRoleList = toRoleListFunction [(3, ["Mafia", "Vanilla Townie", "Supersaint"])],
  validPlayerNumbers = Set.singleton 3,
  allowedToNoLynch = False,
  onHammer = ss3OnHammer}
  where ss3OnHammer hammerer (Just user) game
          | Map.lookup user (playerRoles game) == Just "Mafia"          = endGameEffect ["**The Town has won!**"]
          | Map.lookup user (playerRoles game) == Just "Vanilla Townie" = endGameEffect ["**The Mafia has won!**"]
          | Map.lookup user (playerRoles game) == Just "Supersaint"     =
            case Map.lookup hammerer $ playerRoles game of
              Just "Mafia"          -> endGameEffect ["**The Supersaint kills " `T.append` hammerer `T.append` ", the Mafia! The Town has won!**"]
              Just "Vanilla Townie" -> endGameEffect ["**The Supersaint kills " `T.append` hammerer `T.append` ", the Vanilla Townie! The Mafia has won!**"]
              _                     -> nullGameEffect game
          | otherwise                                                   = endGameEffect ["**The Mafia has won!**"]
        ss3OnHammer _ _ game = nullGameEffect game

wnaf :: Theme
wnaf = defaultTheme {
  getRoleList = toRoleListFunction [(4, ["Mafia Goon", "Mafia Goon", "Vanilla Townie", "Vanilla Townie"])],
  validPlayerNumbers = Set.singleton 4,
  onStart = pmMafiaPartners,
  allowedToNoLynch = False,
  onHammer = wnafOnHammer,
  onTarget = wnafOnTarget}
  where pmMafiaPartners game =
          let mafiaMembers = Map.keys . Map.filter (=="Mafia Goon") $ playerRoles game
          in case mafiaMembers of
            (m1:m2:_) -> messagesEffect
              ["/w " `T.append` m1 `T.append` ", Fellow Mafioso: " `T.append` m2,
               "/w " `T.append` m2 `T.append` ", Fellow Mafioso: " `T.append` m1] game
            _         -> nullGameEffect game
        wnafOnHammer _ (Just user) game =
          case day game of
            1 ->
              case Map.lookup user (playerRoles game) of
                Just "Vanilla Townie" -> mkGameEffect ["**" `T.append` user `T.append` " has been added back to the game! Take your shot with @target room, player**"] $ Just game {players = Set.insert user $ players game, gamePhase = Night, miscInfo = [user, "Vanilla Townie"]}
                Just "Mafia Goon"     -> mkGameEffect ["**" `T.append` user `T.append` ", confirm a town player with @target room, player**"] $ Just game {gamePhase = Night, miscInfo = [user, "Mafia Goon"]}
                _                     -> nullGameEffect game
            2 ->
              case Map.lookup user (playerRoles game) of
                Just "Vanilla Townie" -> endGameEffect ["**The Mafia has won!**"]
                Just "Mafia Goon"     -> endGameEffect ["**The Town has won!**"]
                _                     -> nullGameEffect game
            _ -> nullGameEffect game
        wnafOnHammer _ _ game = nullGameEffect game
        wnafOnTarget user (user2:_) game
          | length (miscInfo game) == 2 && head (miscInfo game) == user && user2 `Set.member` players game =
            case last $ miscInfo game of
              "Vanilla Townie" ->
                case Map.lookup user2 (playerRoles game) of
                  Just "Vanilla Townie" -> endGameEffect ["**" `T.append` user2 `T.append` " was Vanilla Townie!**", "**The Mafia have won!**"]
                  Just "Mafia Goon"     -> mkGameEffect ["**" `T.append` user2 `T.append` " was Mafia Goon!**", "**Day 2 start!**"] $ Just game {players = Set.delete user2 $ players game, day = 2, gamePhase = Day, miscInfo = []}
                  _                     -> nullGameEffect game
              "Mafia Goon"     ->
                if Map.lookup user2 (playerRoles game) == Just "Vanilla Townie"
                  then mkGameEffect ["**" `T.append` user2 `T.append` " has been confirmed as Vanilla Townie**", "**Day 2 start!**"] $ Just game {day = 2, gamePhase = Day, miscInfo = []}
                  else nullGameEffect game
              _                -> nullGameEffect game
          | otherwise = nullGameEffect game
        wnafOnTarget _ _ game = nullGameEffect game
