{-# LANGUAGE LambdaCase #-}

module Utils where

import Data.List (foldl')
import Data.Char (digitToInt)
import qualified Data.Set as Set
import qualified Data.Map as Map

(<$$>) :: Functor f => (a -> b) -> f (f a) -> f (f b)
f <$$> x = fmap (fmap f) x

if' :: Bool -> a -> a -> a
if' b x y = if b then x else y

mapFromSet :: Ord a => (a -> b) -> Set.Set a -> Map.Map a b
mapFromSet f = foldr insertVal Map.empty
  where
    insertVal k m = Map.insert k (f k) m

mapMaxVal :: (Ord k, Ord a) => Map.Map k a -> Maybe (k, a)
mapMaxVal map = foldl' compSnd Nothing list
  where
    list    = Map.toList map
    compSnd mkv1 (k2, v2) = case mkv1 of
      Nothing       -> Just (k2, v2)
      Just (k1, v1)
        | v1 >= v2  -> Just (k1, v1)
        | otherwise -> Just (k2, v2)

mapMinVal :: (Ord k, Ord a) => Map.Map k a -> Maybe (k, a)
mapMinVal map = foldl' compSnd Nothing list
  where
    list    = Map.toList map
    compSnd mkv1 (k2, v2) = case mkv1 of
      Nothing       -> Just (k2, v2)
      Just (k1, v1)
        | v1 <= v2  -> Just (k1, v1)
        | otherwise -> Just (k2, v2)

stringToInt :: String -> Int
stringToInt str = stringToInt_ (length str) str

stringToInt_ :: Int -> String -> Int
stringToInt_ n = \case
  []   -> 0
  x:xs -> ((digitToInt x) * (10^(n-1))) + stringToInt_ (n-1) xs

alphasToInt :: String -> Int
alphasToInt str = alphasToInt_ (length str) str

alphasToInt_ :: Int -> String -> Int
alphasToInt_ n = \case
  []   -> 0
  x:xs -> ((alphaToInt x) * (24^(n-1))) + alphasToInt_ (n-1) xs

alphaToInt :: Char -> Int
alphaToInt x
  | elem x ['a', 'A'] = 0
  | elem x ['b', 'B'] = 1
  | elem x ['c', 'C'] = 2
  | elem x ['d', 'D'] = 3
  | elem x ['e', 'E'] = 4
  | elem x ['f', 'F'] = 5
  | elem x ['g', 'G'] = 6
  | elem x ['h', 'H'] = 7
  | elem x ['i', 'I'] = 8
  | elem x ['j', 'J'] = 9
  | elem x ['k', 'K'] = 10
  | elem x ['l', 'L'] = 11
  | elem x ['m', 'M'] = 12
  | elem x ['n', 'N'] = 13
  | elem x ['o', 'O'] = 14
  | elem x ['p', 'P'] = 15
  | elem x ['q', 'Q'] = 16
  | elem x ['r', 'R'] = 17
  | elem x ['s', 'S'] = 18
  | elem x ['t', 'T'] = 19
  | elem x ['u', 'U'] = 20
  | elem x ['v', 'V'] = 21
  | elem x ['w', 'W'] = 22
  | elem x ['x', 'X'] = 23
  | elem x ['y', 'Y'] = 24
  | elem x ['z', 'Z'] = 25
  | otherwise         = 0
