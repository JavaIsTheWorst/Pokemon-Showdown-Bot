{-# LANGUAGE OverloadedStrings #-}
module Util where

import           Data.Text          as T
import qualified Network.WebSockets as WS

send :: WS.Connection -> T.Text -> IO ()
send = WS.sendTextData

useCommand :: WS.Connection -> T.Text -> IO ()
useCommand conn message = say conn "" message

say :: WS.Connection -> T.Text -> T.Text -> IO ()
say conn room message = send conn $ room `T.append` "|" `T.append` message

pm :: WS.Connection -> T.Text -> T.Text -> IO ()
pm conn username message = send conn $ "|/w " `T.append` username `T.append` "," `T.append` message

toId :: T.Text -> T.Text
toId = T.filter (\c -> c `elem` ['a'..'z'] || c `elem` ['1'..'9']) . T.toLower
