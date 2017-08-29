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
respondWithQuestionMarks = False

sendColor :: (ANSI.ColorIntensity, ANSI.Color)
sendColor = (ANSI.Vivid, ANSI.White)

logColor :: (ANSI.ColorIntensity, ANSI.Color)
logColor = (ANSI.Dull, ANSI.White)

hangmanColor :: (ANSI.ColorIntensity, ANSI.Color)
hangmanColor = (ANSI.Vivid, ANSI.White)

chessColor :: (ANSI.ColorIntensity, ANSI.Color)
chessColor = (ANSI.Vivid, ANSI.Green)

gestiColor :: (ANSI.ColorIntensity, ANSI.Color)
gestiColor = (ANSI.Vivid, ANSI.Yellow)

mafiaColor :: (ANSI.ColorIntensity, ANSI.Color)
mafiaColor = (ANSI.Vivid, ANSI.Cyan)

unoColor :: (ANSI.ColorIntensity, ANSI.Color)
unoColor = (ANSI.Vivid, ANSI.Magenta)

commandColor :: (ANSI.ColorIntensity, ANSI.Color)
commandColor = (ANSI.Vivid, ANSI.White)

connectedColor :: (ANSI.ColorIntensity, ANSI.Color)
connectedColor = (ANSI.Vivid, ANSI.White)

loginColor :: (ANSI.ColorIntensity, ANSI.Color)
loginColor = (ANSI.Vivid, ANSI.White)

startUpColor :: (ANSI.ColorIntensity, ANSI.Color)
startUpColor = (ANSI.Vivid, ANSI.White)