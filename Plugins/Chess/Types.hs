{-# LANGUAGE OverloadedStrings #-}
module Plugins.Chess.Types where

import           Data.Foldable (toList)
import           Data.List     (groupBy)
import           Data.Maybe    (fromJust)
import qualified Data.Sequence as S
import qualified Data.Text     as T

data PieceType = King | Queen | Bishop | Knight | Rook | Pawn deriving (Eq, Show)

fenToPieceType :: Char -> PieceType
fenToPieceType 'K' = King
fenToPieceType 'Q' = Queen
fenToPieceType 'B' = Bishop
fenToPieceType 'N' = Knight
fenToPieceType 'R' = Rook
fenToPieceType 'P' = Pawn
fenToPieceType _ = King

data Color = White | Black deriving (Eq, Show)

letterToColor :: T.Text -> Color
letterToColor "w" = White
letterToColor "b" = Black
letterToColor _   = White

colorToLetter :: Color -> T.Text
colorToLetter White = "w"
colorToLetter Black = "b"

opposite :: Color -> Color
opposite White = Black
opposite Black = White

data Piece = Piece Color PieceType deriving (Eq)

instance Show Piece where
  show (Piece White King) = "K"
  show (Piece White Queen) = "Q"
  show (Piece White Bishop) = "B"
  show (Piece White Knight) = "N"
  show (Piece White Rook) = "R"
  show (Piece White Pawn) = "P"
  show (Piece Black King) = "k"
  show (Piece Black Queen) = "q"
  show (Piece Black Bishop) = "b"
  show (Piece Black Knight) = "n"
  show (Piece Black Rook) = "r"
  show (Piece Black Pawn) = "p"

fenToPiece :: Char -> Maybe Piece
fenToPiece 'K' = Just $ Piece White King
fenToPiece 'Q' = Just $ Piece White Queen
fenToPiece 'B' = Just $ Piece White Bishop
fenToPiece 'N' = Just $ Piece White Knight
fenToPiece 'R' = Just $ Piece White Rook
fenToPiece 'P' = Just $ Piece White Pawn
fenToPiece 'k' = Just $ Piece Black King
fenToPiece 'q' = Just $ Piece Black Queen
fenToPiece 'b' = Just $ Piece Black Bishop
fenToPiece 'n' = Just $ Piece Black Knight
fenToPiece 'r' = Just $ Piece Black Rook
fenToPiece 'p' = Just $ Piece Black Pawn
fenToPiece _ = Nothing

pieceToFen :: Maybe Piece -> Char
pieceToFen (Just piece) =
  case piece of
    Piece White King -> 'K'
    Piece White Queen -> 'Q'
    Piece White Bishop -> 'B'
    Piece White Knight -> 'N'
    Piece White Rook -> 'R'
    Piece White Pawn -> 'P'
    Piece Black King -> 'k'
    Piece Black Queen -> 'q'
    Piece Black Bishop -> 'b'
    Piece Black Knight -> 'n'
    Piece Black Rook -> 'r'
    Piece Black Pawn -> 'p'
pieceToFen Nothing      = ' '

getPieceColor :: Piece -> Color
getPieceColor (Piece c _) = c

getPieceType :: Piece -> PieceType
getPieceType (Piece _ t) = t

data Square = LightSquare (Maybe Piece) | DarkSquare (Maybe Piece) deriving (Eq)

instance Show Square where
  show (LightSquare (Just piece)) = show piece
  show (LightSquare Nothing) = " "
  show (DarkSquare (Just piece)) = show piece
  show (DarkSquare Nothing) = " "

getPiece :: Square -> Maybe Piece
getPiece (LightSquare p) = p
getPiece (DarkSquare p) = p

data Rank = Rank Square Square Square Square Square Square Square Square deriving (Eq)

instance Show Rank where
  show = (>>= show) . toList . rankToList

rankFromList :: S.Seq Square -> Rank
rankFromList squares =
  if S.length squares >= 8
    then Rank (S.index squares 0) (S.index squares 1) (S.index squares 2) (S.index squares 3) (S.index squares 4) (S.index squares 5) (S.index squares 6) (S.index squares 7)
    else error "rankFromList: Given sequence with fewer than 8 squares"

rankToList :: Rank -> S.Seq Square
rankToList (Rank a b c d e f g h) = S.fromList [a,b,c,d,e,f,g,h]

fenToRank :: Int -> T.Text -> Rank
fenToRank rankNumber s = rankFromList . S.fromList $
  let numbersToSpaces = T.replace "1" (T.replicate 1 " ") . T.replace "2" (T.replicate 2 " ") . T.replace "3" (T.replicate 3 " ") . T.replace "4" (T.replicate 4 " ") . T.replace "5" (T.replicate 5 " ") . T.replace "6" (T.replicate 6 " ") . T.replace "7" (T.replicate 7 " ") . T.replace "8" (T.replicate 8 " ")
  in
    if odd rankNumber
      then zipWith ($) (cycle [LightSquare,DarkSquare]) . map fenToPiece . T.unpack $ numbersToSpaces s
      else zipWith ($) (cycle [DarkSquare,LightSquare]) . map fenToPiece . T.unpack $ numbersToSpaces s

rankToFen :: Rank -> T.Text
rankToFen r =
  let textList    = map (pieceToFen . getPiece) . toList . rankToList $ r
      groupedList = groupBy (\a b -> a==' ' && b==' ') textList
  in T.pack $ map (\group ->
    if head group == ' '
      then head . show $ length group
      else head group) groupedList

data Board = Board Rank Rank Rank Rank Rank Rank Rank Rank deriving (Eq)

instance Show Board where
  show = ("\n abcdefgh" ++) . concat . zipWith (\n r -> ('\n':head (show n):show r)) ([1..8] :: [Int]) . toList . boardToList

boardFromList :: S.Seq Rank -> Board
boardFromList ranks =
  if S.length ranks >= 8
    then Board (S.index ranks 0) (S.index ranks 1) (S.index ranks 2) (S.index ranks 3) (S.index ranks 4) (S.index ranks 5) (S.index ranks 6) (S.index ranks 7)
    else error "boardFromList: Given sequence with fewer than 8 ranks"

boardToList :: Board -> S.Seq Rank
boardToList (Board r1 r2 r3 r4 r5 r6 r7 r8) = S.fromList [r1,r2,r3,r4,r5,r6,r7,r8]

fenToBoard :: T.Text -> Board
fenToBoard s =
  let boardLines = S.reverse . S.fromList $ T.splitOn "/" s
  in boardFromList $ S.mapWithIndex fenToRank boardLines

onBoard :: Piece -> Board -> Bool
onBoard piece b =
  let listBoard = rankToList =<< boardToList b
  in any ((==Just piece) . getPiece) listBoard

boardToFen :: Board -> T.Text
boardToFen b =
  let ranks = reverse . toList $ boardToList b
  in T.intercalate "/" $ rankToFen <$> ranks

data Coordinate = Coordinate {file :: Int,
                              rank :: Int} deriving (Eq, Show)

validCoord :: T.Text -> Bool
validCoord s = T.length s == 2 && T.head s `elem` ['a'..'h'] && T.last s `elem` ['1'..'8']

fenToCoord :: T.Text -> Coordinate
fenToCoord s = Coordinate {
  file = (case T.head s of
    'a' -> 1
    'b' -> 2
    'c' -> 3
    'd' -> 4
    'e' -> 5
    'f' -> 6
    'g' -> 7
    'h' -> 8
    _   -> 1),
  rank = (case T.last s of
    '1' -> 1
    '2' -> 2
    '3' -> 3
    '4' -> 4
    '5' -> 5
    '6' -> 6
    '7' -> 7
    '8' -> 8
    _   -> 1)}

coordToFen :: Coordinate -> T.Text
coordToFen coord =
  (case file coord of
    1 -> "a"
    2 -> "b"
    3 -> "c"
    4 -> "d"
    5 -> "e"
    6 -> "f"
    7 -> "g"
    8 -> "h"
    _ -> "a") `T.append`
  (case rank coord of
    1 -> "1"
    2 -> "2"
    3 -> "3"
    4 -> "4"
    5 -> "5"
    6 -> "6"
    7 -> "7"
    8 -> "8"
    _ -> "1")

changeCoord :: Coordinate -> (Int,Int) -> Coordinate
changeCoord coord (f,r) = coord {file=file coord+f,rank=rank coord+r}

coordDiff :: Coordinate -> Coordinate -> (Int, Int)
coordDiff coord1 coord2 =
  (file coord1 - file coord2, rank coord1 - rank coord2)

invalidCoord :: Coordinate -> Bool
invalidCoord coord = file coord > 8 || file coord < 1 || rank coord > 8 || rank coord < 1

whiteKingsideCastleCoord :: Coordinate
whiteKingsideCastleCoord = Coordinate{file=7,rank=1}

whiteQueensideCastleCoord :: Coordinate
whiteQueensideCastleCoord = Coordinate{file=3,rank=1}

blackKingsideCastleCoord :: Coordinate
blackKingsideCastleCoord = Coordinate{file=7,rank=8}

blackQueensideCastleCoord :: Coordinate
blackQueensideCastleCoord = Coordinate{file=3,rank=8}

setCoord :: Board -> Coordinate -> Maybe Piece -> Board
setCoord b coord piece =
  let oldBoard = boardToList b
      oldRank  = rankToList . S.index oldBoard $ rank coord - 1
  in boardFromList $ S.update (rank coord - 1) (rankFromList $ S.adjust (\square ->
    case square of
      LightSquare _ -> LightSquare piece
      DarkSquare _  -> DarkSquare piece) (file coord - 1) oldRank) oldBoard

getCoord :: Board -> Coordinate -> Maybe Piece
getCoord b coord =
  case S.index (rankToList (S.index (boardToList b) $ rank coord - 1)) $ file coord - 1 of
    LightSquare piece -> piece
    DarkSquare piece  -> piece

pieceInWay :: Board -> Coordinate -> Coordinate -> Bool
pieceInWay b coord1 coord2
  | abs (fst $ coordDiff coord1 coord2) == abs (snd $ coordDiff coord1 coord2) =
    pathBlocked $
      zipWith Coordinate
        (if file coord1 < file coord2
          then [file coord1..file coord2]
          else reverse [file coord2..file coord1])
        (if rank coord1 < rank coord2
          then [rank coord1..rank coord2]
          else reverse [rank coord2..rank coord1])
  | fst (coordDiff coord1 coord2) == 0 =
    pathBlocked $
      zipWith Coordinate
        (repeat $ file coord1)
        (if rank coord1 < rank coord2
          then [rank coord1..rank coord2]
          else reverse [rank coord2..rank coord1])
  | snd (coordDiff coord1 coord2) == 0 =
    pathBlocked $
      zipWith Coordinate
        (if file coord1 < file coord2
          then [file coord1..file coord2]
          else reverse [file coord2..file coord1])
        (repeat $ rank coord1)
  | otherwise = error "pieceInWay: coordinates do not line up"
  where pathBlocked path = any (/= Nothing) . init . tail $ map (getCoord b) path

moveIgnoringInvalid :: Board -> Coordinate -> Coordinate -> (Board, Maybe Piece)
moveIgnoringInvalid b coord1 coord2 = (
  setCoord (setCoord b coord1 Nothing) coord2 $ getCoord b coord1,
  getCoord b coord2)

data CastlingAvailability = CastlingAvailability {kingside :: Bool,
                                                  queenside :: Bool} deriving (Eq, Show)

data Position = Position {board :: Board,
                          colorToMove :: Color,
                          whiteCanCastle :: CastlingAvailability,
                          blackCanCastle :: CastlingAvailability,
                          enPassantTargetSquare :: Maybe Coordinate,
                          halfmoveClock :: Int,
                          fullmoveClock :: Int} deriving (Eq, Show)

fenToPosition :: T.Text -> Position
fenToPosition fen =
  let parts                  = T.splitOn " " fen
      board'                 = fenToBoard $ parts !! 0
      colorToMove'           = letterToColor $ parts !! 1
      whiteCanCastle'        = CastlingAvailability {kingside = "K" `T.isInfixOf` (parts !! 2), queenside = "Q" `T.isInfixOf` (parts !! 2)}
      blackCanCastle'        = CastlingAvailability {kingside = "k" `T.isInfixOf` (parts !! 2), queenside = "q" `T.isInfixOf` (parts !! 2)}
      enPassantTargetSquare' =
        if (parts !! 3) == "-"
          then Nothing
          else Just . fenToCoord $ parts !! 3
      halfmoveClock'         = read . T.unpack $ parts !! 4
      fullmoveClock'         = read . T.unpack $ parts !! 5
  in Position {board = board', colorToMove = colorToMove', whiteCanCastle = whiteCanCastle', blackCanCastle = blackCanCastle', enPassantTargetSquare = enPassantTargetSquare', halfmoveClock = halfmoveClock', fullmoveClock = fullmoveClock'}

initialFEN :: T.Text
initialFEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

initialPosition :: Position
initialPosition = fenToPosition initialFEN

positionToFen :: Position -> T.Text
positionToFen pos =
  let board'                 = boardToFen $ board pos
      colorToMove'           = colorToLetter $ colorToMove pos
      whiteCanCastle'        =
        (if kingside $ whiteCanCastle pos
          then "K"
          else "") `T.append`
        (if queenside $ whiteCanCastle pos
          then "Q"
          else "")
      blackCanCastle'        =
        (if kingside $ blackCanCastle pos
          then "k"
          else "") `T.append`
        (if queenside $ blackCanCastle pos
          then "q"
          else "")
      castlingRights         =
        if whiteCanCastle' `T.append` blackCanCastle' == ""
          then "-"
          else whiteCanCastle' `T.append` blackCanCastle'
      enPassantTargetSquare' = case enPassantTargetSquare pos of
        Just coord -> coordToFen coord
        _          -> "-"
      halfmoveClock'         = T.pack . show $ halfmoveClock pos
      fullmoveClock'         = T.pack . show $ fullmoveClock pos
  in T.intercalate " " [board', colorToMove', castlingRights, enPassantTargetSquare', halfmoveClock', fullmoveClock']

data KingMoveType = Normal | KingsideCastle | QueensideCastle deriving (Eq)

kingMove :: Coordinate -> [(Coordinate,KingMoveType)]
kingMove coord = [(newCoord,Normal) | diff <- [(0,-1),(1,-1),(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1)], let newCoord = changeCoord coord diff, not $ invalidCoord newCoord]

queenMove :: Coordinate -> [Coordinate]
queenMove coord = bishopMove coord ++ rookMove coord

bishopMove :: Coordinate -> [Coordinate]
bishopMove coord = [newCoord | diff <- zip [-7 .. -1] (reverse [1 .. 7]) ++ zip [-7 .. -1] [-7 .. -1] ++ zip (reverse [1 .. 7]) [-7 .. -1] ++ zip (reverse [1 .. 7]) (reverse [1 .. 7]), let newCoord = changeCoord coord diff, not $ invalidCoord newCoord]

rookMove :: Coordinate -> [Coordinate]
rookMove coord = [newCoord | diff <- zip [-7 .. -1] (repeat 0) ++ zip (repeat 0) [1 .. 7] ++ zip [1 .. 7] (repeat 0) ++ zip (repeat 0) [-7 .. -1], let newCoord = changeCoord coord diff, not $ invalidCoord newCoord]

knightMove :: Coordinate -> [Coordinate]
knightMove coord = [newCoord | diff <- [(1,-2),(2,-1),(2,1),(1,2),(-1,2),(-2,1),(-2,-1),(-1,-2)], let newCoord = changeCoord coord diff, not $ invalidCoord newCoord]

pawnCanAdvanceTwo :: Color -> Coordinate -> Bool
pawnCanAdvanceTwo White coord = rank coord == 2
pawnCanAdvanceTwo Black coord = rank coord == 7

pawnDirection :: Color -> Int -> Int
pawnDirection White = id
pawnDirection Black = negate

pawnCanCapture :: Color -> Coordinate -> Coordinate -> Bool
pawnCanCapture White coord1 coord2 =
  coordDiff coord1 coord2 == (-1,-1) ||
  coordDiff coord1 coord2 == (1,-1)
pawnCanCapture Black coord1 coord2 =
  coordDiff coord1 coord2 == (-1,1) ||
  coordDiff coord1 coord2 == (1,1)

data PawnMoveType = ForwardTwo | ForwardOne | Capture | EnPassant deriving (Eq, Show)

pawnMove :: Board -> Color -> Coordinate -> [(Coordinate,PawnMoveType)]
pawnMove b color coord = map (\(c,pt) -> (fromJust c,pt)) . filter ((/= Nothing) . fst) $
  [((if getCoord b forwardOneCoord == Nothing
    then Just forwardOneCoord
    else Nothing),ForwardOne),
   ((if not (invalidCoord leftCaptureCoord) && getCoord b leftCaptureCoord /= Nothing
    then Just leftCaptureCoord
    else Nothing),Capture),
   ((if not (invalidCoord rightCaptureCoord) && getCoord b rightCaptureCoord /= Nothing
    then Just rightCaptureCoord
    else Nothing),Capture),
   ((if pawnCanAdvanceTwo color coord && getCoord b forwardOneCoord == Nothing && getCoord b forwardTwoCoord == Nothing
    then Just forwardTwoCoord
    else Nothing),ForwardTwo)]
  where forwardOneCoord   = changeCoord coord (0,pawnDirection color 1)
        leftCaptureCoord  = changeCoord coord (-1,pawnDirection color 1)
        rightCaptureCoord = changeCoord coord (1,pawnDirection color 1)
        forwardTwoCoord   = changeCoord coord (0,pawnDirection color 2)

genMoves :: Position -> Coordinate -> [Position]
genMoves pos@(Position {colorToMove=White}) coord =
  case getCoord (board pos) coord of
    Just piece ->
      case piece of
        Piece Black _      -> []
        Piece White King   ->
          let kingCoords      =
                (if kingside $ whiteCanCastle pos
                  then ((whiteKingsideCastleCoord,KingsideCastle):)
                  else id) $
                (if queenside $ whiteCanCastle pos
                  then ((whiteQueensideCastleCoord,QueensideCastle):)
                  else id) $
                kingMove coord
              validKingCoords = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos) . fst) $ filter (\(_,moveType) ->
                case moveType of
                  Normal          -> True
                  KingsideCastle  -> not $ (pieceInWay (board pos) coord (Coordinate {file=8,rank=1})) || (inCheck White . fst $ moveIgnoringInvalid (board pos) coord (Coordinate {file=6,rank=1})) || inCheck White (board pos)
                  QueensideCastle -> not $ (pieceInWay (board pos) coord (Coordinate {file=1,rank=1})) || (inCheck White . fst $ moveIgnoringInvalid (board pos) coord (Coordinate {file=4,rank=1})) || inCheck White (board pos)) kingCoords
              validKingMoves  = map (\(coord',moveType) ->
                case moveType of
                  Normal          -> moveIgnoringInvalid (board pos) coord coord'
                  KingsideCastle  -> moveIgnoringInvalid (fst $ moveIgnoringInvalid (board pos) (Coordinate{file=8,rank=1}) (Coordinate{file=6,rank=1})) coord coord'
                  QueensideCastle -> moveIgnoringInvalid (fst $ moveIgnoringInvalid (board pos) (Coordinate{file=1,rank=1}) (Coordinate{file=4,rank=1})) coord coord') validKingCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=Black,whiteCanCastle=CastlingAvailability{kingside=False,queenside=False},enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0}) validKingMoves
        Piece White Queen  ->
          let queenCoords      = queenMove coord
              validQueenCoords = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) queenCoords
              validQueenMoves  = map (moveIgnoringInvalid (board pos) coord) validQueenCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=Black,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0}) validQueenMoves
        Piece White Bishop ->
          let bishopCoords      = bishopMove coord
              validBishopCoords = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) bishopCoords
              validBishopMoves  = map (moveIgnoringInvalid (board pos) coord) validBishopCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=Black,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0}) validBishopMoves
        Piece White Knight ->
          let knightCoords      = knightMove coord
              validKnightCoords = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos)) knightCoords
              validKnightMoves  = map (moveIgnoringInvalid (board pos) coord) validKnightCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=Black,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0}) validKnightMoves
        Piece White Rook   ->
          let rookCoords               = rookMove coord
              validRookCoords          = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) rookCoords
              validRookMoves           = map (moveIgnoringInvalid (board pos) coord) validRookCoords
              castlingAvailability old =
                case coord of
                  Coordinate {file=1,rank=1} -> CastlingAvailability {kingside=kingside old,queenside=False}
                  Coordinate {file=8,rank=1} -> CastlingAvailability {kingside=False,queenside=queenside old}
                  _                          -> old
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=Black,whiteCanCastle=castlingAvailability $ whiteCanCastle pos,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0}) validRookMoves
        Piece White Pawn   ->
          let pawnCoords      =
                case enPassantTargetSquare pos of
                  Just enPassantCoord ->
                    if pawnCanCapture White coord enPassantCoord
                      then (enPassantCoord,EnPassant) : pawnMove (board pos) White coord
                      else pawnMove (board pos) White coord
                  _                   -> pawnMove (board pos) White coord
              validPawnCoords = filter (maybe True ((/= White) . getPieceColor) . getCoord (board pos) . fst) pawnCoords
              validPawnMoves  = map (\(coord',moveType) ->
                case moveType of
                  EnPassant  -> (coord', moveIgnoringInvalid (setCoord (board pos) (changeCoord coord' (0,pawnDirection White (-1))) Nothing) coord coord', Nothing)
                  ForwardTwo -> (coord', moveIgnoringInvalid (board pos) coord coord', Just $ changeCoord coord (0,pawnDirection White 1))
                  _          -> (coord', moveIgnoringInvalid (board pos) coord coord', Nothing)) validPawnCoords
              validPawnMovesWithPromotion = do
                (coord',(b,_),target) <- validPawnMoves
                if rank coord' == 8
                  then zip [setCoord b coord' (Just $ Piece White Bishop),
                            setCoord b coord' (Just $ Piece White Knight),
                            setCoord b coord' (Just $ Piece White Rook),
                            setCoord b coord' (Just $ Piece White Queen)] $ repeat Nothing
                  else return (b,target)
          in map (\(b,target) -> pos {board=b,colorToMove=Black,enPassantTargetSquare=target,halfmoveClock=0}) validPawnMovesWithPromotion
    _          -> []
genMoves pos@(Position {colorToMove=Black}) coord =
  case getCoord (board pos) coord of
    Just piece ->
        case piece of
        Piece White _      -> []
        Piece Black King   ->
          let kingCoords      =
                (if kingside $ blackCanCastle pos
                  then ((blackKingsideCastleCoord,KingsideCastle):)
                  else id) $
                (if queenside $ blackCanCastle pos
                  then ((blackQueensideCastleCoord,QueensideCastle):)
                  else id) $
                kingMove coord
              validKingCoords = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos) . fst) $ filter (\(_,moveType) ->
                case moveType of
                  Normal          -> True
                  KingsideCastle  -> not $ (pieceInWay (board pos) coord (Coordinate {file=8,rank=8})) || (inCheck Black . fst $ moveIgnoringInvalid (board pos) coord (Coordinate {file=6,rank=8})) || inCheck Black (board pos)
                  QueensideCastle -> not $ (pieceInWay (board pos) coord (Coordinate {file=1,rank=8})) || (inCheck Black . fst $ moveIgnoringInvalid (board pos) coord (Coordinate {file=4,rank=8})) || inCheck Black (board pos)) kingCoords
              validKingMoves  = map (\(coord',moveType) ->
                case moveType of
                  Normal          -> moveIgnoringInvalid (board pos) coord coord'
                  KingsideCastle  -> moveIgnoringInvalid (fst $ moveIgnoringInvalid (board pos) (Coordinate{file=8,rank=8}) (Coordinate{file=6,rank=8})) coord coord'
                  QueensideCastle -> moveIgnoringInvalid (fst $ moveIgnoringInvalid (board pos) (Coordinate{file=1,rank=8}) (Coordinate{file=4,rank=8})) coord coord') validKingCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=White,blackCanCastle=CastlingAvailability{kingside=False,queenside=False},enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0,fullmoveClock=fullmoveClock pos+1}) validKingMoves
        Piece Black Queen  ->
          let queenCoords      = queenMove coord
              validQueenCoords = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) queenCoords
              validQueenMoves  = map (moveIgnoringInvalid (board pos) coord) validQueenCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=White,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0,fullmoveClock=fullmoveClock pos+1}) validQueenMoves
        Piece Black Bishop ->
          let bishopCoords      = bishopMove coord
              validBishopCoords = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) bishopCoords
              validBishopMoves  = map (moveIgnoringInvalid (board pos) coord) validBishopCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=White,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0,fullmoveClock=fullmoveClock pos+1}) validBishopMoves
        Piece Black Knight ->
          let knightCoords      = knightMove coord
              validKnightCoords = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos)) knightCoords
              validKnightMoves  = map (moveIgnoringInvalid (board pos) coord) validKnightCoords
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=White,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0,fullmoveClock=fullmoveClock pos+1}) validKnightMoves
        Piece Black Rook   ->
          let rookCoords               = rookMove coord
              validRookCoords          = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos)) $ filter (not . pieceInWay (board pos) coord) rookCoords
              validRookMoves           = map (moveIgnoringInvalid (board pos) coord) validRookCoords
              castlingAvailability old =
                case coord of
                  Coordinate {file=1,rank=8} -> CastlingAvailability {kingside=kingside old,queenside=False}
                  Coordinate {file=8,rank=8} -> CastlingAvailability {kingside=False,queenside=queenside old}
                  _                          -> old
          in map (\(b,oldPiece) -> pos {board=b,colorToMove=White,blackCanCastle=castlingAvailability $ blackCanCastle pos,enPassantTargetSquare=Nothing,halfmoveClock=if oldPiece==Nothing then halfmoveClock pos+1 else 0,fullmoveClock=fullmoveClock pos+1}) validRookMoves
        Piece Black Pawn   ->
          let pawnCoords      =
                case enPassantTargetSquare pos of
                  Just enPassantCoord ->
                    if pawnCanCapture Black coord enPassantCoord
                      then (enPassantCoord,EnPassant) : pawnMove (board pos) Black coord
                      else pawnMove (board pos) Black coord
                  _                   -> pawnMove (board pos) Black coord
              validPawnCoords = filter (maybe True ((/= Black) . getPieceColor) . getCoord (board pos) . fst) pawnCoords
              validPawnMoves  = map (\(coord',moveType) ->
                case moveType of
                  EnPassant  -> (coord', moveIgnoringInvalid (setCoord (board pos) (changeCoord coord' (0,pawnDirection Black (-1))) Nothing) coord coord', Nothing)
                  ForwardTwo -> (coord', moveIgnoringInvalid (board pos) coord coord', Just $ changeCoord coord (0,pawnDirection Black 1))
                  _          -> (coord', moveIgnoringInvalid (board pos) coord coord', Nothing)) validPawnCoords
              validPawnMovesWithPromotion = do
                (coord',(b,_),target) <- validPawnMoves
                if rank coord' == 1
                  then zip [setCoord b coord' (Just $ Piece Black Bishop),
                            setCoord b coord' (Just $ Piece Black Knight),
                            setCoord b coord' (Just $ Piece Black Rook),
                            setCoord b coord' (Just $ Piece Black Queen)] $ repeat Nothing
                  else return (b,target)
          in map (\(b,target) -> pos {board=b,colorToMove=White,enPassantTargetSquare=target,halfmoveClock=0,fullmoveClock=fullmoveClock pos+1}) validPawnMovesWithPromotion
    _          -> []

inCheck :: Color -> Board -> Bool
inCheck color board' =
  let coords = [Coordinate f r | f <- [1..8], r <- [1..8]]
  in any (not . (Piece color King `onBoard`)) $ board <$> (genMoves (Position {board=board',colorToMove=opposite color,whiteCanCastle=CastlingAvailability{kingside=False,queenside=False},blackCanCastle=CastlingAvailability{kingside=False,queenside=False},enPassantTargetSquare=Nothing,halfmoveClock=0,fullmoveClock=0}) =<< coords)

movePiece :: Coordinate -> Coordinate -> Maybe PieceType -> Position -> Either T.Text Position
movePiece coord1 coord2 promoteTo pos =
  let piece         = getCoord (board pos) coord1
      promoteTo'    = Piece (getPieceColor $ fromJust piece) <$> promoteTo
      legalMoves    = genMoves pos coord1
      matchingMoves
        | piece /= Nothing && getPieceType (fromJust piece) == Pawn && rank coord2 == 8 =
          if promoteTo == Nothing
            then []
            else [move | move <- legalMoves, getCoord (board pos) coord2 /= promoteTo', getCoord (board move) coord2 == promoteTo']
        | otherwise                              =
          [move | move <- legalMoves, getCoord (board pos) coord2 /= piece, getCoord (board move) coord2 == piece]
      process
        | null matchingMoves = Left "That is not a legal move."
        | otherwise          = Right $ head matchingMoves
      accountCheck pos'
        | inCheck (colorToMove pos) $ board pos' = Left "That move moves your king into check."
        | otherwise                              = Right pos'
  in accountCheck =<< process

data ChessGame = ChessGame {
  players :: (T.Text, T.Text),
  position :: Position,
  moves :: T.Text}