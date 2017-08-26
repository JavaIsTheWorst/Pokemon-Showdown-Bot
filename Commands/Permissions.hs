module Commands.Permissions where

import           Data.List                     (isPrefixOf)
import qualified Data.Text                     as T
import qualified System.IO.Strict              as S

import           Util

toIdS :: String -> String
toIdS = T.unpack . toId . T.pack

demote :: String -> IO ()
demote user = do
  permissions <- S.readFile "permissions.txt"
  let filteredPermsList = filter (not . ((toIdS user ++ ": ") `isPrefixOf`)) $ lines permissions
      filteredPerms     = unlines filteredPermsList
  writeFile "permissions.txt" filteredPerms

demoteText :: T.Text -> IO ()
demoteText = demote . T.unpack

setRank :: String -> Integer -> IO ()
setRank user permission = do
  permissions <- S.readFile "permissions.txt"
  let filteredPermsList = filter (not . ((toIdS user ++ ": ") `isPrefixOf`)) $ lines permissions
      updatedPerms      = unlines $ (toIdS user ++ ": " ++ show permission) : filteredPermsList
  writeFile "permissions.txt" updatedPerms

setRankText :: T.Text -> Integer -> IO ()
setRankText = setRank . T.unpack

getRank :: String -> IO Integer
getRank user = do
  permissions <- S.readFile "permissions.txt"
  let searchedPermission = filter ((toIdS user ++ ": ") `isPrefixOf`) $ lines permissions
  if null searchedPermission
    then return 0
    else return . read . drop (length (toIdS user) + 2) . head $ searchedPermission

getRankText :: T.Text -> IO Integer
getRankText = getRank . T.unpack

checkRank :: String -> (Integer -> Bool) -> IO () -> IO () -> IO ()
checkRank user predicate thenAction elseAction = do
  permission <- getRank user
  if predicate permission
    then thenAction
    else elseAction

checkRankText :: T.Text -> (Integer -> Bool) -> IO () -> IO () -> IO ()
checkRankText = checkRank . T.unpack