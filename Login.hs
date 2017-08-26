{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Login where

import           Control.Monad.IO.Class        (liftIO)
import           Control.Monad.Trans.Reader    (ReaderT)
import           Data.Aeson                    (eitherDecode, ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Network.HTTP.Base             (urlEncode)
import           Network.HTTP.Conduit          (RequestBody (RequestBodyLBS))
import           Network.HTTP.Simple           (addRequestHeader, getResponseBody, httpLBS, parseRequest, setRequestBody)
import           GHC.Generics                  (Generic)
import qualified Data.ByteString.Lazy.Char8    as B
import qualified Data.Text                     as T

import qualified Config
import           Util

onChallstrMessage :: T.Text -> ReaderT Env IO () -> ReaderT Env IO ()
onChallstrMessage msg continuation = do
  initRequest <- parseRequest $ "POST " ++ Config.actionUrl
  let challstr    = T.unpack $ T.drop (T.length "|challstr|") msg
      postData    = "act=login&name=" ++ urlEncode Config.username ++ "&pass=" ++ urlEncode Config.password ++ "&challstr=" ++ urlEncode challstr
      request     = setRequestBody (RequestBodyLBS $ B.pack postData) . addRequestHeader "content-type" "application/x-www-form-urlencoded" $ initRequest
  response <- liftIO $ httpLBS request
  let assertionStr = either (error . show) assertion . eitherDecode . B.tail $ getResponseBody response
      loginCommand = "/trn " ++ Config.username ++ ",0," ++ assertionStr
  useGlobalCommand Config.loginColor $ T.pack loginCommand
  continuation

data AssertionObject = AssertionObject {assertion :: String} deriving (Generic, Show)

instance ToJSON AssertionObject where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON AssertionObject