{-# LANGUAGE OverloadedStrings #-}
module Config where

import qualified Data.Text                 as T
import qualified System.Console.ANSI       as ANSI

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
username = "113 (mod 71)"

password :: String
password = "This is a different password"

owner :: String
owner = "Java SE Runtime"

defaultRoom :: T.Text
defaultRoom = head rooms

rooms :: [T.Text]
rooms = ["mafia", "chess", "math"]

harassList :: [T.Text]
harassList = ["Java SE Runtime", "RADicate", "Zorquax"]

respondWithQuestionMarks :: Bool
respondWithQuestionMarks = True

logColor :: ANSI.Color
logColor = ANSI.White

hangmanColor :: ANSI.Color
hangmanColor = ANSI.White

chessColor :: ANSI.Color
chessColor = ANSI.Green

gestiColor :: ANSI.Color
gestiColor = ANSI.Yellow

unoColor :: ANSI.Color
unoColor = ANSI.Magenta

commandColor :: ANSI.Color
commandColor = ANSI.White

connectedColor :: ANSI.Color
connectedColor = ANSI.White

loginColor :: ANSI.Color
loginColor = ANSI.White

startUpColor :: ANSI.Color
startUpColor = ANSI.White