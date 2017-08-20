{-# LANGUAGE OverloadedStrings #-}
module Plugins.Hangman where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.List                  (intersperse)
import qualified Data.Text                  as T

import qualified Config
import           Util

onHangmanMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onHangmanMessage room _ = do
  env <- ask
  let guessesReaderT   = map (say Config.hangmanColor room . ("/hangman guess " `T.append`)) ["o","d","m"]
      guesses          = map (($ env) . runReaderT) guessesReaderT
      guessesWithDelay = intersperse (threadDelay $ 600 * 1000) guesses
  liftIO . forkIO $ sequence_ guessesWithDelay
  return ()
