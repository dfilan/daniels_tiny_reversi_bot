{-# LANGUAGE TupleSections, LambdaCase #-}

module Reversi (
  Player(..),
  Move(..),
  RState,
  boardSize,
  Result(..),
  startBoard,
  playMove,
  getLegalMoves,
  isLegalMove,
  isEndState,
  countTokens,
  getResult,
  swapPlayer,
  numStable
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

data RState = RState { boardSize :: Int,
                       boardMap  :: Map.Map Location Token
                     }
  deriving (Eq, Ord)

instance Show RState where
  show (RState n b) = foldl' (\ x y -> x ++ "\n" ++ y) "" catRows
  -- this will look ugly if n >= 26
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
    n = boardSize s
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
  | not $ isInRange (loc m) s                  = False
  | Map.lookup (loc m) (boardMap s) /= Nothing = False
  | not $ Set.null $ getMatches m s            = True
  | otherwise                                  = False

isInRange :: Location -> RState -> Bool
isInRange (x,y) s
  | x < 0                = False
  | y < 0                = False
  | x >= 2 * boardSize s = False
  | y >= 2 * boardSize s = False
  | otherwise            = True

capInDir :: Move -> RState -> (Int, Int) -> Maybe Location
capInDir m s (x,y) = capInDir_ 0 m s (x,y)

capInDir_ :: Int -> Move -> RState -> (Int, Int) -> Maybe Location
capInDir_ n m s (x,y) = case Map.lookup nextLoc $ boardMap s of
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
playMove s m = if' (isLegalMove s m) flipState{boardMap=addedToken} s
  where
    matches    = getMatches m s
    locsToFlip = Set.unions $ Set.map (positionsBetween $ loc m) matches
    flipState  = foldr flipLoc s locsToFlip
    addedToken =
      Map.insert (loc m) (playerToToken $ player m) $ boardMap flipState

flipLoc :: Location -> RState -> RState
flipLoc l s = s{boardMap = Map.adjust swapTok l $ boardMap s}

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
    ts = catMaybes $ (flip Map.lookup $ boardMap s) <$> Set.toList ls

getResult :: RState -> Maybe Result
getResult s
  | not $ isEndState s     = Nothing
  | numBlacks > numWhites  = Just BlackWin
  | numWhites > numBlacks  = Just WhiteWin
  | numBlacks == numWhites = Just Tie
  where
    numBlacks = countTokens s BlackTok
    numWhites = countTokens s WhiteTok

numStable :: RState -> Player -> Int
numStable s p = Set.size $ Set.filter (isStable s) myLocs
  where
    isMine loc = Map.lookup loc (boardMap s) == Just (playerToToken p)
    myLocs     = Set.filter isMine $ boardLocs s

isStable :: RState -> Location -> Bool
isStable s loc = and $ stableInDirection s loc <$> directions
  where
    directions = [(-1, -1), (-1, 0), (-1, 1), (0, 1)]
-- you're stable if in each cardinal direction, either there's one orientation
-- where you go through pieces of your own colour until you hit a wall,
-- or you're already 'sandwiched' between two pieces of the opposite colour.
-- (i.e. in both orientations, you hit a piece of the opposite colour before
-- you hit an empty grid location)

stableInDirection :: RState -> Location -> (Int, Int) -> Bool
stableInDirection s (x,y) (dx, dy) = case Map.lookup (x,y) $ boardMap s of
  Nothing -> False
  Just t  -> hitWall s t (x,y) (dx, dy) || sandwiched s t (x,y) (dx, dy)

hitWall :: RState -> Token -> Location -> (Int, Int) -> Bool
hitWall s t (x,y) (dx, dy) =
  hitWall_ True s t (x,y) (dx, dy) || hitWall_ False s t (x,y) (dx, dy)

hitWall_ :: Bool -> RState -> Token -> Location -> (Int, Int) -> Bool
hitWall_ goPos s t (x,y) (dx, dy)
  | not $ isInRange nextLoc s = True
  | otherwise                 = case Map.lookup nextLoc $ boardMap s of
      Nothing -> False
      Just t_ -> if' (t_ /= t) False $ hitWall_ goPos s t nextLoc (dx, dy)
  where
    f       = if' goPos (+) $ flip (-)
    nextLoc = bimap (f dx) (f dy) (x,y)

sandwiched :: RState -> Token -> Location -> (Int, Int) -> Bool
sandwiched  s t (x, y) (dx, dy) =
  sandwiched_ True s t (x, y) (dx, dy) && sandwiched_ False s t (x, y) (dx, dy)

sandwiched_ :: Bool -> RState -> Token -> Location -> (Int, Int) -> Bool
sandwiched_ goPos s t (x, y) (dx, dy) = case Map.lookup nextLoc $ boardMap s of
  Nothing -> False
  Just t_ -> if' (t_ /= t) True $ sandwiched_ goPos s t nextLoc (dx, dy)
  where
    f       = if' goPos (+) $ flip (-)
    nextLoc = bimap (f dx) (f dy) (x,y)
