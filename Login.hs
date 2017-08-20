{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Login where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT)
import           Data.Aeson                    (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Data.Text.Encoding            (encodeUtf8)
import           Network.HTTP                  (simpleHTTP, postRequestWithBody, getResponseBody)
import           Network.HTTP.Base             (urlEncode, rspReason)
import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy          as B
import qualified Data.Text                     as T

import qualified Config
import           Util

onChallstrMessage :: T.Text -> ReaderT Env IO () -> ReaderT Env IO ()
onChallstrMessage msg continuation = do
  let challstr    = T.unpack $ T.drop (T.length "|challstr|") msg
      postData    = "act=login&name=" ++ urlEncode Config.username ++ "&pass=" ++ urlEncode Config.password ++ "&challstr=" ++ urlEncode challstr
      contentType = "application/x-www-form-urlencoded"
  httpResponse <- liftIO $ simpleHTTP (postRequestWithBody Config.actionUrl contentType postData)
  (_:jsonData) <- liftIO $ getResponseBody httpResponse
  let loginCommand = "/trn " ++ Config.username ++ ",0," ++ getAssertion jsonData
  useCommand Config.loginColor $ T.pack loginCommand
  continuation

data AssertionObject = AssertionObject {assertion :: String} deriving (Generic, Show)

instance ToJSON AssertionObject where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AssertionObject

getAssertion :: String -> String
getAssertion = assertion . either error id . eitherDecode . toLazyByteString
  where toLazyByteString = B.fromStrict . encodeUtf8 . T.pack
