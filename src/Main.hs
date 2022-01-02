{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Char (isDigit, isAlpha)
import Control.Monad (liftM2)
import qualified Data.Set as Set

import Utils
import Reversi
import Agent

type Settings = (Player, Int)
-- TODO: let player choose heuristic from predefined list.

main :: IO ()
main = do
  (size, (agent, depth)) <- promptSettings
  putStrLn $ "The initial board:"
  let startState = startBoard size
  putStrLn $ show startState
  loopPlay (agent, depth) startState Black

promptSettings :: IO (Int, Settings)
promptSettings = do
  putStrLn "What size board would you like to play on?"
  putStrLn "Type an integer - press 8 to play on a standard board."
  sizeStr <- promptUntil (all isDigit) "Sorry, please enter only digits."
  let size = floor $ (fromIntegral $ stringToInt sizeStr) / 2.0
  putStrLn "Would you like to play as Black or White?"
  putStrLn "Type B or W (without the quotation marks)."
  let playError = "Sorry, please type B or W."
  playerStr <- promptUntil (liftM2 (||) ("B" == ) ("W" ==)) playError
  let user = if' (playerStr == "B") Black White
  let agent = swapPlayer user
  putStrLn "How deep should the minimax search go for?"
  depthStr <- promptUntil (all isDigit) "Sorry, please enter only digits."
  let depth = stringToInt depthStr
  return (size, (agent, depth))
  
promptUntil :: (String -> Bool) -> String -> IO String
promptUntil test error = do
  input <- getLine
  case test input of
    True  -> return input
    False -> putStrLn error >> promptUntil test error

loopPlay :: Settings -> RState -> Player -> IO ()
loopPlay settings state player
  | isEndState state = endGame state
  | player == agent  = agentMove settings state
  | otherwise        = userMove settings state
  where
    agent = fst settings
  
endGame :: RState -> IO ()
endGame state = case getResult state of
  Nothing -> putStrLn "The game isn't actually over - this should never happen"
  Just BlackWin -> putStrLn "Black has won - thanks for playing!"
  Just WhiteWin -> putStrLn "White has won - thanks for playing!"
  Just Tie      -> putStrLn "It's a tie - thanks for playing!"

agentMove :: Settings -> RState -> IO ()
agentMove (agent, d) state
  | Set.null $ getLegalMoves state agent = do
      putStrLn "\nThe bot has no available moves."
      loopPlay (agent, d) state $ swapPlayer agent
  | otherwise =
      case minimaxMoveVal d simpleHeur agent state of
        Nothing          ->
          putStrLn "\nThe bot has no legal moves - this should never happen"
        Just (move, val) -> do
          let nextState = playMove state move
          putStrLn "\nThe bot's move:"
          putStrLn $ show nextState
          putStrLn $ "Game value: " ++ show val
          loopPlay (agent, d) nextState $ swapPlayer agent

userMove :: Settings -> RState -> IO ()
userMove (agent, d) state
  | Set.null $ getLegalMoves state (swapPlayer agent) = do
      putStrLn "\nYou have no legal moves. It is now the bot's move."
      loopPlay (agent, d) state agent
  | otherwise = do
      putStrLn "\nPlease input a move, such as 'a1'"
      let stringTest =
            liftM2 (&&) isCoord ((isLegalMove state)
                                 . (readMove $ swapPlayer agent))
          coordError =
            "Input a coordinate that constitutes a legal move for you."
      moveStr <- promptUntil stringTest coordError
      let move      = readMove (swapPlayer agent) moveStr
          nextState = playMove state move
      putStrLn "\nYour move:"
      putStrLn $ show nextState
      loopPlay (agent, d) nextState agent

isCoord :: String -> Bool
isCoord = (all isDigit) . snd . (span isAlpha)

readMove :: Player -> String -> Move
readMove p str = Move p (xCoord, yCoord)
  where
    (letters, nums) = span isAlpha str
    xCoord          = alphasToInt letters
    numPart         = takeWhile isDigit nums
    yCoord          = (stringToInt numPart) - 1
    