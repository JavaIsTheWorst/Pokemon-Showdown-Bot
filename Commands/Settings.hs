module Commands.Settings where

import           Data.List                     (isPrefixOf)
import qualified System.IO.Strict              as S

setSetting :: String -> String -> String -> IO ()
setSetting setting room newSetting = do
  settings <- S.readFile "settings.txt"
  let filteredSettingsList = filter (not . (setting `isPrefixOf`)) $ lines settings
      updatedSettings      = unlines $ (setting ++ ": " ++ room ++ ": " ++ newSetting) : filteredSettingsList
  writeFile "settings.txt" updatedSettings

getSetting :: String -> String -> IO (Maybe String)
getSetting setting room = do
  settings <- S.readFile "settings.txt"
  let searchedSetting = filter ((setting ++ ": " ++ room) `isPrefixOf`) $ lines settings
      roomSetting     = if null searchedSetting then filter ((setting ++ ": default") `isPrefixOf`) $ lines settings else searchedSetting
  if null roomSetting
    then return Nothing
    else return . Just . drop (length room + 2) . drop (length setting + 2) . head $ roomSetting

checkSetting :: String -> String -> (Maybe String -> Bool) -> IO () -> IO () -> IO ()
checkSetting setting room predicate thenAction elseAction = do
  setting' <- getSetting setting room
  if predicate setting'
    then thenAction
    else elseAction
