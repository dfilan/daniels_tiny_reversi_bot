{-# LANGUAGE LambdaCase #-}

module Agent (
  Value,
  minimaxMoveVal,
  simpleHeur
  ) where

import Data.List (sortOn)
import Data.Random.Distribution
import qualified Data.Set as Set
import qualified Data.Map as Map

import Utils
import Reversi

-- Black maximizes, White minimizes

type Value = Float

endValue :: Result -> Value
endValue = \case
  BlackWin -> 1.0
  Tie      -> 0.5
  WhiteWin -> 0.0
-- heuristics should agree with endValue . getResult

incorporateResult :: (RState -> Value) -> RState -> Value
incorporateResult heur s = case getResult s of
  Nothing -> heur s
  Just r  -> endValue r

simpleHeur :: RState -> Value
simpleHeur = incorporateResult $ const 0.5
  -- TODO: incorporate MCTS

minimaxMoveVal ::
  Int -> (RState -> Value) -> Player -> RState -> Maybe (Move, Value)
minimaxMoveVal d heur p s = case p of
  Black -> mapMaxVal valMap
  White -> mapMinVal valMap
  where
    moves   = getLegalMoves s p
    nextVal :: Move -> Value
    nextVal = minimaxValue d heur (swapPlayer p) (-1.0) (2.0) . playMove s
    valMap  = mapFromSet nextVal moves

minimaxValue ::
  Int -> (RState -> Value) -> Player -> Value -> Value -> RState -> Value
minimaxValue d heur p a b s
  | d == 0         = heur s
  | isEndState s   = heur s
  | Set.null moves = minimaxValue d heur (swapPlayer p) a b s
  | otherwise      = case p of
      Black -> maxVal d heur a b nextStates
      White -> minVal d heur a b nextStates
  where
    moves      = getLegalMoves s p
    nextStates = Set.map (playMove s) moves

maxVal :: Int -> (RState -> Value) -> Value -> Value -> Set.Set RState -> Value
maxVal d heur a b = maxVal_ d heur a b (-1.0) . (enqueueStates Black heur)

maxVal_ ::
  Int ->
  (RState -> Value) ->
  Value -> -- alpha
  Value -> -- beta
  Value -> -- v
  [RState] ->
  Value
maxVal_ d heur a b v = \case
  [] -> v
  x:xs
    | v_ >= b   -> v_
    | otherwise -> maxVal_ d heur a_ b v_ xs
    where
      xVal = minimaxValue (d - 1) heur White a b x
      v_   = max v xVal
      a_   = max a v_

minVal :: Int -> (RState -> Value) -> Value -> Value -> Set.Set RState -> Value
minVal d heur a b = minVal_ d heur a b (2.0) . (enqueueStates White heur)

minVal_ ::
  Int ->
  (RState -> Value) ->
  Value -> -- alpha
  Value -> -- beta
  Value -> -- v
  [RState] ->
  Value
minVal_ d heur a b v = \case
  [] -> v
  x:xs
    | v_ <= a   -> v_
    | otherwise -> minVal_ d heur a b_ v_ xs
    where
      xVal = minimaxValue (d - 1) heur Black a b x
      v_   = min v xVal
      b_   = min b v_

enqueueStates :: Player -> (RState -> Value) -> Set.Set RState -> [RState]
enqueueStates p heur = sortOn ranker . Set.toList
  where
    ranker = case p of
      Black -> (* (-1)) . heur
      White -> heur

numLegalMoves :: RState -> Player -> Int
numLegalMoves = (Set.size .) . getLegalMoves

legalMovesHeur :: RState -> Value
legalMovesHeur = incorporateResult legalMovesHeur_

legalMovesHeur_ :: RState -> Value
legalMovesHeur_ s = bMoves / (bMoves + wMoves)
  where
    bMoves = fromIntegral $ numLegalMoves s Black
    wMoves = fromIntegral $ numLegalMoves s White

stabilityHeur :: RState -> Value
stabilityHeur = incorporateResult stabilityHeur_

stabilityHeur_ :: RState -> Value
stabilityHeur_ s = pBlackWin + (0.5 * pTie)
  where
    numStableBlack = numStable s Black
    numStableWhite = numStable s White
    totalNum       = (2 * (boardSize s))^2
    numUnclear     = totalNum - numStableBlack - numStableWhite
    threshNum      = 0.5 * (totalNum + numWhite - numBlack)
    pWhiteWin      = integralBinomialCDF numUnclear 0.5 $ threshNum - 1
    pTie           = integralBinomialPDF numUnclear 0.5 threshNum
    pBlackWin      = 1.0 - pWhiteWin - pTie
    -- P(White wins) == P(X + numBlack < (totalNum - X) + numWhite)
    -- where X ~ Binom(numUnclear, 0.5) is the num of unclear squares that turn
    -- black.
    -- manipuulating that, P(White wins)
    -- == P(2*X + numBlack < totalNum + numWhite)
    -- == P(X < 0.5*(totalNum + numWhite - numBlack))

mixStabMovesHeurs :: Float -> RState -> Value
mixStabMovesHeurs p s = p * (stabilityHeur s) + (1-p) * (legalMovesHeur s)

-- further TODO: read stuart abt quiescence or ordering or whatever
