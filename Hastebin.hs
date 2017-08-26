{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Hastebin where

import           Control.Monad.Trans.Reader (ReaderT)
import           Data.Aeson                 (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import           Data.Text.Encoding         (encodeUtf8)
import           GHC.Generics               (Generic)
import           Network.HTTP.Conduit       (RequestBody (RequestBodyBS))
import           Network.HTTP.Simple        (addRequestHeader, getResponseBody, httpJSONEither, parseRequest, setRequestBody)
import qualified Data.Text                  as T

import           Util

upload :: T.Text -> ReaderT Env IO T.Text
upload text = do
  initRequest <- parseRequest "POST https://hastebin.com/documents"
  let request = setRequestBody (RequestBodyBS $ encodeUtf8 text) . addRequestHeader "content-type" "text/plain" $ initRequest
  response <- httpJSONEither request
  return . either (error . show) (("https://hastebin.com/raw/" `T.append`) . key) $ getResponseBody response

data KVPair = KVPair {key :: T.Text} deriving (Generic, Show)

instance ToJSON KVPair where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON KVPair