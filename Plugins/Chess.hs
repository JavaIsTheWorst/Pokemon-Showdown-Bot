{-# LANGUAGE OverloadedStrings #-}
module Plugins.Chess where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Concurrent.MVar    (modifyMVar_, readMVar, swapMVar)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Foldable              (foldr1)
import           Data.Maybe                 (fromJust)
import qualified Data.Sequence              as S
import qualified Data.Text                  as T

import qualified Config
import           Plugins.Chess.Types
import           Util

size :: Int
size = 48

halfSquareSize :: T.Text
halfSquareSize = T.pack . show $ size `div` 2

squareSize :: T.Text
squareSize = T.pack $ show size

boardSize :: T.Text
boardSize = T.pack . show $ size * 8

htmlPiece :: Piece -> T.Text
htmlPiece (Piece White King) = "♔"
htmlPiece (Piece White Queen) = "♕"
htmlPiece (Piece White Bishop) = "♗"
htmlPiece (Piece White Knight) = "♘"
htmlPiece (Piece White Rook) = "♖"
htmlPiece (Piece White Pawn) = "♙"
htmlPiece (Piece Black King) = "♚"
htmlPiece (Piece Black Queen) = "♛"
htmlPiece (Piece Black Bishop) = "♝"
htmlPiece (Piece Black Knight) = "♞"
htmlPiece (Piece Black Rook) = "♜"
htmlPiece (Piece Black Pawn) = "♟"

htmlSquare :: Coordinate -> Square -> T.Text
htmlSquare coord (LightSquare Nothing) = "<div style=\"display:block;padding:0px;margin:0px;width:" `T.append` squareSize `T.append` "px;height:" `T.append` squareSize `T.append` "px;float:left;background-color:#FFFFFF;position:relative;color:#000000;\" title=\"" `T.append` coordToFen coord `T.append` "\"></div>"
htmlSquare coord (LightSquare (Just piece)) = "<div style=\"display:block;padding:0px;margin:0px;text-align:center;width:" `T.append` squareSize `T.append`"px;height:" `T.append` squareSize `T.append` "px;float:left;background-color:#FFFFFF;font-size:" `T.append` squareSize `T.append` "px;line-height:" `T.append` squareSize `T.append` "px;position:relative;color:#000000;\" title=\"" `T.append` coordToFen coord `T.append` "\">" `T.append` htmlPiece piece `T.append` "</div>"
htmlSquare coord (DarkSquare Nothing) = "<div style=\"display:block;padding:0px;margin:0px;width:" `T.append` squareSize `T.append` "px;height:" `T.append` squareSize `T.append` "px;float:left;background-color:#DD5500;position:relative;color:#000000;\" title=\"" `T.append` coordToFen coord `T.append` "\"></div>"
htmlSquare coord (DarkSquare (Just piece)) = "<div style=\"display:block;padding:0px;margin:0px;text-align:center;width:" `T.append` squareSize `T.append`"px;height:" `T.append` squareSize `T.append` "px;float:left;background-color:#DD5500;font-size:" `T.append` squareSize `T.append` "px;line-height:" `T.append` squareSize `T.append` "px;position:relative;color:#000000;\" title=\"" `T.append` coordToFen coord `T.append` "\">" `T.append` htmlPiece piece `T.append` "</div>"

htmlRank :: Color -> Int -> Rank -> T.Text
htmlRank White rankNumber r =
  "<div>" `T.append` (foldr1 T.append . S.zipWith (\fileNumber -> htmlSquare $ Coordinate fileNumber rankNumber) (S.fromList [1..8]) $ rankToList r) `T.append` "</div>"
htmlRank Black rankNumber r =
  "<div>" `T.append` (foldr1 T.append . S.zipWith (\fileNumber -> htmlSquare $ Coordinate fileNumber rankNumber) (S.fromList [8,7..1]) . S.reverse $ rankToList r) `T.append` "</div>"

htmlBoard :: Board -> Color -> T.Text
htmlBoard b White =
  "<div style=\"display:block;padding:0px;margin:0px;width:" `T.append` boardSize `T.append` "px;height:" `T.append` boardSize `T.append` "px;border:4px solid #FF0000;\">" `T.append` (foldr1 T.append . S.reverse . S.zipWith (htmlRank White) (S.fromList [1..8]) $ boardToList b) `T.append` "</div>"
htmlBoard b Black =
  "<div style=\"display:block;padding:0px;margin:0px;width:" `T.append` boardSize `T.append` "px;height:" `T.append` boardSize `T.append` "px;border:4px solid #000000;\">" `T.append` (foldr1 T.append . S.zipWith (htmlRank Black) (S.fromList [1..8]) $ boardToList b) `T.append` "</div>"

chessRoom :: T.Text
chessRoom = "groupchat-" `T.append` toId (T.pack Config.username) `T.append` "-botchess"

onInitiateMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onInitiateMessage whitePlayer blackPlayer = do
  env <- ask
  _ <- liftIO . forkIO $ do
    runReaderT (sayInDefault Config.chessColor "/makegroupchat botchess") env
    threadDelay $ 600 * 1000
    runReaderT (useGlobalCommand Config.chessColor $ "/j " `T.append` chessRoom) env
    runReaderT (say Config.chessColor chessRoom $ "/roomvoice " `T.append` whitePlayer) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "/roomvoice " `T.append` blackPlayer) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "/invite " `T.append` whitePlayer) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "/invite " `T.append` blackPlayer) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "Starting a new Bot Chess game between " `T.append` whitePlayer `T.append` " and " `T.append` blackPlayer `T.append` "!") env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "!htmlbox " `T.append` htmlBoard (board initialPosition) White) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ "FEN: " `T.append` initialFEN) env
    threadDelay $ 600 * 1000
    runReaderT (say Config.chessColor chessRoom $ whitePlayer `T.append` "'s turn!") env
    _ <- swapMVar (chessGame env) $ Just (ChessGame{players=(whitePlayer,blackPlayer),position=initialPosition,moves=""})
    return ()
  return ()

getGame :: ReaderT Env IO (Maybe ChessGame)
getGame = ask >>= liftIO . readMVar . chessGame

getPosition :: ReaderT Env IO (Maybe Position)
getPosition = do
  env <- ask
  game <- liftIO . readMVar $ chessGame env
  return $ position <$> game

setPosition :: Position -> ReaderT Env IO ()
setPosition pos' = do
  env <- ask
  liftIO . modifyMVar_ (chessGame env) $ return . ((\game -> game {position=pos'}) <$>)

getMoves :: ReaderT Env IO (Maybe T.Text)
getMoves = do
  env <- ask
  game <- liftIO . readMVar $ chessGame env
  return $ moves <$> game

setMoves :: T.Text -> ReaderT Env IO ()
setMoves moves' = do
  env <- ask
  liftIO . modifyMVar_ (chessGame env) $ return . ((\game -> game {moves=moves'}) <$>)

addMove :: T.Text -> ReaderT Env IO ()
addMove move = do
  env <- ask
  let appendMove existingMoves move' =
        if T.null existingMoves
          then move'
          else existingMoves `T.append` " " `T.append` move'
      changeMoves maybeGame =
        case maybeGame of
          Just game -> return . Just $ game {moves=appendMove move $ moves game}
          Nothing   -> return Nothing
  liftIO $ modifyMVar_ (chessGame env) changeMoves

onChessMoveMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onChessMoveMessage user move = do
  maybeGame <- getGame
  case maybeGame of
    Just game@(ChessGame{players=(whitePlayer,blackPlayer)}) ->
      let userColor =
            (if user == whitePlayer
              then (White:)
              else id) $
            (if user == blackPlayer
              then (Black:)
              else id)
            []
      in if (colorToMove $ position game) `elem` userColor
        then do
          pos' <-
            case processMove $ position game of
              Left err -> do
                say Config.chessColor chessRoom err
                return $ position game
              Right pos' -> do
                setPosition pos'
                addMove move
                return pos'
          moves' <- getMoves
          say Config.chessColor chessRoom $ "!htmlbox " `T.append` htmlBoard (board pos') (colorToMove pos')
          liftIO . threadDelay $ 600 * 1000
          say Config.chessColor chessRoom $ "FEN: " `T.append` positionToFen pos'
          liftIO . threadDelay $ 600 * 1000
          say Config.chessColor chessRoom $ "Moves: " `T.append` fromJust moves'
          liftIO . threadDelay $ 600 * 1000
          case colorToMove pos' of
            White -> say Config.chessColor chessRoom $ whitePlayer `T.append` "'s turn!"
            Black -> say Config.chessColor chessRoom $ blackPlayer `T.append` "'s turn!"
        else return ()
    Nothing                                                                           ->
      return ()
  where processMove pos =
          let startCoord      = T.take 2 move
              startCoordValid
                | validCoord startCoord = Just $ fenToCoord startCoord
                | otherwise             = Nothing
              endCoord        = T.take 2 $ T.drop 2 move
              endCoordValid
                | validCoord endCoord = Just $ fenToCoord endCoord
                | otherwise           = Nothing
              pawnPromoTo
                | T.length move == 4 = Nothing
                | otherwise          = Just . fenToPieceType . T.head $ T.drop 4 move
          in case (startCoordValid, endCoordValid) of
            (Just coord1, Just coord2) -> movePiece coord1 coord2 pawnPromoTo pos
            _                          -> Left "Invalid move"

onImportFENMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onImportFENMessage user fen = do
  game <- getGame
  case game of
    Just (ChessGame{players=(whitePlayer,blackPlayer),position=_,moves=_}) ->
      if user == whitePlayer || user == blackPlayer
        then do
          let pos = fenToPosition fen
          setPosition pos
          liftIO . threadDelay $ 600 * 1000
          say Config.chessColor chessRoom $ "!htmlbox " `T.append` htmlBoard (board pos) (colorToMove pos)
          liftIO . threadDelay $ 600 * 1000
          say Config.chessColor chessRoom $ "FEN: " `T.append` fen
          liftIO . threadDelay $ 600 * 1000
          case colorToMove pos of
            White -> say Config.chessColor chessRoom $ whitePlayer `T.append` "'s turn! Move with ``move: <startsquare><endsquare>``, e.g. ``move: e2e4``."
            Black -> say Config.chessColor chessRoom $ blackPlayer `T.append` "'s turn! Move with ``move: <startsquare><endsquare>``, e.g. ``move: e7e5``."
        else return ()
    Nothing                                                                -> return ()

onImportMovesMessage :: T.Text -> T.Text -> ReaderT Env IO ()
onImportMovesMessage user moves' = do
  maybeGame <- getGame
  case maybeGame of
    Just (ChessGame{players=(whitePlayer,blackPlayer)}) ->
      if user == whitePlayer || user == blackPlayer
        then setMoves moves'
        else return ()
    Nothing                                             -> return ()

onChessEndMessage :: ReaderT Env IO ()
onChessEndMessage = do
  env <- ask
  maybeGame <- getGame
  case maybeGame of
    Just game@(ChessGame{players=(whitePlayer,blackPlayer)}) -> do
      _ <- liftIO . forkIO $ do
        runReaderT (useGlobalCommand Config.chessColor $ "/j " `T.append` chessRoom) env
        threadDelay $ 600 * 1000
        runReaderT (say Config.chessColor chessRoom $ "/roomdeauth " `T.append` whitePlayer) env
        threadDelay $ 600 * 1000
        runReaderT (say Config.chessColor chessRoom $ "/roomdeauth " `T.append` blackPlayer) env
        threadDelay $ 600 * 1000
        runReaderT (say Config.chessColor chessRoom $ "Ending FEN: " `T.append` positionToFen (position game)) env
        threadDelay $ 600 * 1000
        runReaderT (say Config.chessColor chessRoom $ "Moves: " `T.append` moves game) env
      return ()
    Nothing                                                  ->
      return ()