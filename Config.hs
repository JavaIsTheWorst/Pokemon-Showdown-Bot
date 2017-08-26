{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text as T

server :: String
server = "sim.smogon.com"

port :: Int
port = 8000

serverId :: String
serverId = "showdown"

actionUrl :: String
actionUrl = "http://play.pokemonshowdown.com/~~" ++ serverId ++ "/action.php"

path :: String
path = "/" ++ serverId ++ "/websocket"

username :: String
username = --Fill in with desired username

password :: String
password = --Fill in with account password to bot

owner :: String
owner = --Your username

rooms :: [T.Text]
rooms = --List of rooms to join

harassList :: [T.Text]
harassList = --List of usernames to PM upon startup
