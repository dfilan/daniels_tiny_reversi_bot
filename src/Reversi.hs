{-# LANGUAGE TupleSections, LambdaCase #-}

module Reversi (
  Player(..),
  Move(..),
  RState(..),
  Result(..),
  startBoard,
  playMove,
  getLegalMoves,
  isLegalMove,
  isEndState,
  countTokens,
  getResult,
  swapPlayer
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
import Control.Applicative (liftA2)

import Utils

data Player = Black | White deriving (Eq, Ord, Show)

data Token = BlackTok | WhiteTok deriving (Eq, Ord, Show)

type Location = (Int, Int)

data Move = Move { player :: Player, loc :: Location } deriving (Eq, Ord, Show)

data RState = RState { size  :: Int,
                       board :: Map.Map Location Token }
  deriving (Eq, Ord)

instance Show RState where
  show (RState n b) = foldl' (\ a b -> a ++ "\n" ++ b) "" catRows
  -- this will look ugly if n >= 24
    where
      numDigs    = 1 + (floor $ logBase 10.0 $ fromIntegral (2*n))
      numAlphas  = 1 + (floor $ logBase 24.0 $ fromIntegral (2*n))
      range      = [0..(2*n - 1)]
      boardLocs  = [[(x,y) | x <- range] | y <- range]
      locsTokens = showLoc <$$> ((flip Map.lookup $ b) <$$> boardLocs)
      showNum n  = (replicate (numDigs - length (show n)) ' ') ++ show n
      h          :: (Int -> a) -> [[a]] -> [[a]]
      h          = h_ 1
      h_         :: Int -> (Int -> a) -> [[a]] -> [[a]]
      h_ n f     = \case
        []   -> []
        x:xs -> ((f n):x) : h_ (n+1) f xs
      rows       = h showNum locsTokens
      firstRow   = replicate numDigs ' ' : getFileNames (2*n)
      rows_      = firstRow : rows
      addSpace x = (replicate (numAlphas - length x) ' ') ++ x
      addSpaces  = \case
        []   -> []
        x:xs -> x : (map addSpace xs)
      rows__     = map addSpaces rows_
      catRows    = concat <$> rows__

data Result = BlackWin | WhiteWin | Tie deriving (Eq, Show)

playerToToken :: Player -> Token
playerToToken = \case
  Black -> BlackTok
  White -> WhiteTok

tokenToPlayer :: Token -> Player
tokenToPlayer = \case
  BlackTok -> Black
  WhiteTok -> White

swapTok :: Token -> Token
swapTok = \case
  BlackTok -> WhiteTok
  WhiteTok -> BlackTok

swapPlayer :: Player -> Player
swapPlayer = \case
  Black -> White
  White -> Black

showLoc :: Maybe Token -> String
showLoc = \case
  Nothing       -> "-"
  Just BlackTok -> "B"
  Just WhiteTok -> "W"

getFileNames :: Int -> [String]
getFileNames = (flip take) cartPrds
  where
    alphabet = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m",
                "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z"]
    prodSelf = liftA2 (++) alphabet
    cartPrds = concat $ iterate prodSelf alphabet
    
startBoard :: Int -> RState
startBoard n = RState n $ Map.fromList [((n-1, n-1), WhiteTok),
                                        ((n-1, n), BlackTok),
                                        ((n, n-1), BlackTok),
                                        ((n,n), WhiteTok)]

boardLocs :: RState -> Set.Set Location
boardLocs s = Set.cartesianProduct range range
  where
    n = size s
    range = Set.fromList [0..(2*n - 1)]

intsBetween :: Int -> Int -> [Int]
intsBetween n m
  | n <= m    = [(n+1)..(m-1)]
  | otherwise = reverse [(m+1)..(n-1)]

positionsBetween :: Location -> Location -> Set.Set Location
positionsBetween (p1, p2) (q1, q2)
  | p1 == q1 = Set.fromList $ map (p1,) $ intsBetween p2 q2
  | p2 == q2 = Set.fromList $ map (,p2) $ intsBetween p1 q1
  | p1 - q1 == p2 - q2 = Set.fromList $ zip (intsBetween p1 q1)
                         (intsBetween p2 q2)
  | p1 - q1 == q2 - p2 = Set.fromList $ zip (intsBetween p1 q1)
                         (intsBetween p2 q2)
  | otherwise = Set.empty

isLegalMove :: RState -> Move -> Bool
isLegalMove s m
  | not $ isInRange m s                     = False
  | Map.lookup (loc m) (board s) /= Nothing = False
  | not $ Set.null $ getMatches m s         = True
  | otherwise                               = False

isInRange :: Move -> RState -> Bool
isInRange m s
  | fst (loc m) < 0           = False
  | snd (loc m) < 0           = False
  | fst (loc m) >= 2 * size s = False
  | snd (loc m) >= 2 * size s = False
  | otherwise                 = True

capInDir :: Move -> RState -> (Int, Int) -> Maybe Location
capInDir m s (x,y) = capInDir_ 0 m s (x,y)

capInDir_ :: Int -> Move -> RState -> (Int, Int) -> Maybe Location
capInDir_ n m s (x,y) = case Map.lookup nextLoc $ board s of
  Nothing -> Nothing
  Just t  -> case (t == (playerToToken $ player m)) of
    True  -> if' (n == 0) Nothing $ Just nextLoc
    False -> capInDir_ (n+1) m{loc=nextLoc} s (x,y)
  where
    nextLoc = bimap (+x) (+y) $ loc m

getMatches :: Move -> RState -> Set.Set Location
getMatches m s = Set.fromList $ catMaybes $ capInDir m s <$> dirList
  where
    dirList = [(-1, -1), (-1, 0), (-1, 1),
               (0, -1), (0, 1),
               (1, -1), (1, 0), (1, 1)]

playMove :: RState -> Move -> RState
playMove s m = if' (isLegalMove s m) flipState{board=addedToken} s
  where
    matches    = getMatches m s
    locsToFlip = Set.unions $ Set.map (positionsBetween $ loc m) matches
    flipState  = foldr flipLoc s locsToFlip
    addedToken = Map.insert (loc m) (playerToToken $ player m) $ board flipState

flipLoc :: Location -> RState -> RState
flipLoc l s = RState (size s) $ Map.adjust swapTok l $ board s

getLegalMoves :: RState -> Player -> Set.Set Move
getLegalMoves s p = Set.filter (isLegalMove s) allMoves
  where
    boardLocs_ = boardLocs s
    allMoves   = Set.map (Move p) boardLocs_

isEndState :: RState -> Bool
isEndState s = ((foldr Set.union Set.empty $ getLegalMoves s <$> [Black, White])
                == Set.empty)

countTokens :: RState -> Token -> Int
countTokens s t = length $ filter (== t) ts
  where
    ls = boardLocs s
    ts = catMaybes $ (flip Map.lookup $ board s) <$> Set.toList ls

getResult :: RState -> Maybe Result
getResult s
  | not $ isEndState s     = Nothing
  | numBlacks > numWhites  = Just BlackWin
  | numWhites > numBlacks  = Just WhiteWin
  | numBlacks == numWhites = Just Tie
  where
    numBlacks = countTokens s BlackTok
    numWhites = countTokens s WhiteTok
