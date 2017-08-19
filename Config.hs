{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text                 as T
import qualified System.Console.ANSI       as ANSI
import qualified System.Console.ANSI.Types as ANSI

import           Util

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
harassList = --List of username to PM upon startup

logColor :: ANSI.Color
logColor = ANSI.Blue

hangmanColor :: ANSI.Color
hangmanColor = ANSI.White

unoColor :: ANSI.Color
unoColor = ANSI.Green

commandColor :: ANSI.Color
commandColor = ANSI.White

connectedColor :: ANSI.Color
connectedColor = ANSI.White

loginColor :: ANSI.Color
loginColor = ANSI.White

startUpColor :: ANSI.Color
startUpColor = ANSI.White
