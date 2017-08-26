{-# LANGUAGE OverloadedStrings #-}
module Plugins.GreatestIdea where

import           Control.Concurrent         (threadDelay)
import           Control.Concurrent.MVar    (modifyMVar_, readMVar, swapMVar)
import           Control.Monad              (join)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask)
import           Data.List                  (nub)
import           Data.Maybe                 (fromJust)
import           System.Random              (newStdGen, randomRs)
import qualified Data.Map.Strict            as Map
import qualified Data.Text                  as T

import qualified Config
import qualified Hastebin
import           Plugins.GreatestIdea.Types
import           Util

onInitiateMessage :: T.Text -> [T.Text] -> ReaderT Env IO ()
onInitiateMessage hostParam playersParam = do
  env <- ask
  _ <- liftIO . swapMVar (gestiGame env) $ Just (GestiGame{host=hostParam,players=playersParam,assigned=Map.empty,choices=Map.fromList . zip playersParam $ repeat Nothing})
  roles <- liftIO $ map (roleList !!) . nub . randomRs (0,length roleList-1) <$> newStdGen
  sequence_ . zipWith (\n user -> do
    liftIO $ modifyMVar_ (gestiGame env) (\(Just game) -> return . Just $
      game {assigned=Map.insert user (roles !! (3*n), roles !! (3*n+1), roles !! (3*n+2)) $ assigned game})
    pm Config.gestiColor user $ "Options: " `T.append` T.intercalate "; " (prettyShowRole <$> [roles !! (3*n), roles !! (3*n+1), roles !! (3*n+2)])
    pm Config.gestiColor user $ "Choose your role with ``gestipick: <alignment#(from 1-3)>, <role#(from 1-3)>``, e.g. ``gestipick: 2, 1``"
    liftIO . threadDelay $ 1000 * 1000) [0..] $ playersParam

onPickMessage :: T.Text -> T.Text -> T.Text -> ReaderT Env IO ()
onPickMessage user alignmentNumber roleNumber = do
  env <- ask
  maybeGame <- liftIO . readMVar $ gestiGame env
  case maybeGame of
    Just game ->
      if user `elem` players game
        then
          let first (a,_,_)  = a
              second (_,b,_) = b
              third (_,_,c)  = c
              assignedRoles = fromJust . Map.lookup user $ assigned game
              maybeChosenAlignment = alignment <$>
                case alignmentNumber of
                  "1" -> Just $ first assignedRoles
                  "2" -> Just $ second assignedRoles
                  "3" -> Just $ third assignedRoles
                  _ -> Nothing
              maybeChosenRoleType  = roleType <$>
                case roleNumber of
                  "1" -> Just $ first assignedRoles
                  "2" -> Just $ second assignedRoles
                  "3" -> Just $ third assignedRoles
                  _ -> Nothing
              maybeDiscard =
                case (alignmentNumber, roleNumber) of
                  ("1","2") -> Just $ third assignedRoles
                  ("1","3") -> Just $ second assignedRoles
                  ("2","1") -> Just $ third assignedRoles
                  ("2","3") -> Just $ first assignedRoles
                  ("3","1") -> Just $ second assignedRoles
                  ("3","2") -> Just $ first assignedRoles
                  _ -> Nothing
          in
            case (maybeChosenAlignment, maybeChosenRoleType, maybeDiscard) of
              (Just chosenAlignment, Just chosenRoleType, Just chosenDiscard) -> do
                pm Config.gestiColor user $ "You have chosen the role: " `T.append` prettyShowRole (Role{alignment=chosenAlignment,roleType=chosenRoleType})
                _ <- liftIO . swapMVar (gestiGame env) . Just $
                  game {
                    choices=Map.adjust (const . Just $ Choice {
                      choice=Role{alignment=chosenAlignment,roleType=chosenRoleType},
                      discard=chosenDiscard})
                    user $ choices game}
                return ()
              _                                                         ->
                pm Config.gestiColor user $ "Choose your role with ``gestipick: <alignment#(from 1-3)>, <role#(from1-3)>``, e.g. ``gestipick: 2, 1``. You cannot pick the same role for alignment and role."
        else return ()
    _                                                                   -> return ()

onGestiEndMessage :: ReaderT Env IO ()
onGestiEndMessage = do
  env <- ask
  maybeGame <- liftIO . readMVar $ gestiGame env
  case maybeGame of
    Just game -> do
      let showRoleTuple (a,b,c)         = T.intercalate ", " [prettyShowRole a, prettyShowRole b, prettyShowRole c]
          didNotChooseMessage player    = player `T.append` " did not choose. Options were: " `T.append` showRoleTuple (fromJust . Map.lookup player $ assigned game)
          choseMessage player choice'   = player `T.append` " chose: " `T.append` prettyShowRole (choice choice')
          discardMessage player choice' = player `T.append` " discarded: " `T.append` prettyShowRole (discard choice')
      rolesURL   <- Hastebin.upload $ T.intercalate ";\n" [maybe (didNotChooseMessage player) (choseMessage player) . join . Map.lookup player $ choices game | player <- players game]
      discardURL <- Hastebin.upload $ T.intercalate ";\n" [maybe (didNotChooseMessage player) (discardMessage player) . join . Map.lookup player $ choices game | player <- players game]
      pm Config.gestiColor (host game) $ "Roles: " `T.append` rolesURL
      pm Config.gestiColor (host game) $ "Discards: " `T.append` discardURL
    Nothing                                                            -> return ()
  _ <- liftIO $ swapMVar (gestiGame env) Nothing
  return ()