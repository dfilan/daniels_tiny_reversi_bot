{-# LANGUAGE LambdaCase, TupleSections #-}

module Main where

import Data.Char (isDigit, isAlpha)
import Control.Monad (liftM2)
import qualified Data.Set as Set
import Data.Composition

import Utils
import Reversi
import Agent

type InteractiveSettings = (Player, RState -> Value, Int)
-- TODO: let player choose heuristic from predefined list.

main :: IO ()
main = do
  putStrLn "Type I to interact with a bot, or type T to run a bot tournament."
  let choiceError = "Sorry, please type I or T"
  choice <- promptUntil (flip elem ["i", "I", "t", "T"]) choiceError
  if' (elem choice ["i", "I"]) playWithBot playTournament

playWithBot :: IO ()
playWithBot = do
  (size, (agent, heur, depth)) <- promptInteractiveSettings
  putStrLn $ "The initial board:"
  let startState = startBoard size
  putStrLn $ show startState
  loopPlay (agent, heur, depth) startState Black

playTournament :: IO ()
playTournament = do
  (n, d) <- promptSizeDepth
  let tournamentResults = tournamentSizeDepth n d
  putStrLn "Tournament results: [uninformed, legalMoves, stability, mix]"
  putStrLn $! show tournamentResults

promptSizeDepth :: IO (Int, Int)
promptSizeDepth = do
  size <- getSize
  depth <- getDepth
  return (size, depth)

getSize :: IO Int
getSize = do
  putStrLn "What size board would you like to use?"
  putStrLn "Type an integer - press 8 to use a standard board."
  sizeStr <- promptUntil (all isDigit) "Sorry, please enter only digits."
  return $ floor $ (fromIntegral $ stringToInt sizeStr) / 2.0

getDepth :: IO Int
getDepth = do
  putStrLn "How deep should the minimax search go for?"
  depthStr <- promptUntil (all isDigit) "Sorry, please enter only digits."
  return $ stringToInt depthStr
  
promptInteractiveSettings :: IO (Int, InteractiveSettings)
promptInteractiveSettings = do
  size <- getSize
  putStrLn "Would you like to play as Black or White?"
  putStrLn "Type B or W (without the quotation marks)."
  let playError = "Sorry, please type B or W."
  playerStr <- promptUntil (flip elem ["b", "B", "w", "W"]) playError
  let user   = if' (elem playerStr ["B", "b"]) Black White
      agent  = swapPlayer user
      heurs  = ["u", "U", "l", "L", "s", "S", "m", "M"]
      hError = "Sorry, please type U, L, S, or M."
  putStrLn "Would you like to play against the uninformed agent (U), the legalMoves agent (L), the stableTokens agent (S), or the mixture agent (M)?"
  heurStr <- promptUntil (flip elem heurs) hError
  let heur
        | elem heurStr ["u", "U"] = simpleHeur
        | elem heurStr ["l", "L"] = legalMovesHeur
        | elem heurStr ["s", "S"] = stabilityHeur
        | elem heurStr ["m", "M"] = mixStabMovesHeurs 0.5
        | otherwise               = simpleHeur
  depth <- getDepth
  return (size, (agent, heur, depth))
  
promptUntil :: (String -> Bool) -> String -> IO String
promptUntil test error = do
  input <- getLine
  case test input of
    True  -> return input
    False -> putStrLn error >> promptUntil test error

loopPlay :: InteractiveSettings -> RState -> Player -> IO ()
loopPlay settings state player
  | isEndState state = endGame state
  | player == agent  = agentMove settings state
  | otherwise        = userMove settings state
  where
    fst3 (x,y,z) = x
    agent        = fst3 settings
  
endGame :: RState -> IO ()
endGame state = case getResult state of
  Nothing -> putStrLn "The game isn't actually over - this should never happen"
  Just BlackWin -> putStrLn "Black has won - thanks for playing!"
  Just WhiteWin -> putStrLn "White has won - thanks for playing!"
  Just Tie      -> putStrLn "It's a tie - thanks for playing!"

agentMove :: InteractiveSettings -> RState -> IO ()
agentMove (agent, heur, d) state
  | Set.null $ getLegalMoves state agent = do
      putStrLn "\nThe bot has no available moves."
      loopPlay (agent, heur, d) state $ swapPlayer agent
  | otherwise =
      case minimaxMoveVal d heur agent state of
        Nothing          ->
          putStrLn "\nThe bot has no legal moves - this should never happen"
        Just (move, val) -> do
          let nextState = playMove state move
          putStrLn "\nThe bot's move:"
          putStrLn $ show nextState
          putStrLn $ "Game value: " ++ show val
          loopPlay (agent, heur, d) nextState $ swapPlayer agent

userMove :: InteractiveSettings -> RState -> IO ()
userMove (agent, heur, d) state
  | Set.null $ getLegalMoves state (swapPlayer agent) = do
      putStrLn "\nYou have no legal moves. It is now the bot's move."
      loopPlay (agent, heur, d) state agent
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
      loopPlay (agent, heur, d) nextState agent

isCoord :: String -> Bool
isCoord = (all isDigit) . snd . (span isAlpha)

readMove :: Player -> String -> Move
readMove p str = Move p (xCoord, yCoord)
  where
    (letters, nums) = span isAlpha str
    xCoord          = alphasToInt letters
    numPart         = takeWhile isDigit nums
    yCoord          = (stringToInt numPart) - 1

allAgents :: Int -> [Player -> RState -> Maybe Move]
allAgents depth = ((.:) (fmap fst)) <$> minimaxMoveVal depth <$> heurList
  where
    heurList = [simpleHeur, legalMovesHeur, stabilityHeur,
                mixStabMovesHeurs 0.5
               ]

tournamentSizeDepth :: Int -> Int -> [Value]
tournamentSizeDepth n d = scorePlayerN resultLists <$> [0..(numPlayers - 1)]
  where
    pairsList         = getTournPairs $ allAgents d
    -- simp              = (fmap fst) .: minimaxMoveVal 3 simpleHeur
    -- leg               = (fmap fst) .: minimaxMoveVal 3 legalMovesHeur
    -- playPair (a1, a2) = playAgents n simp leg
    playPair (a1, a2) = playAgents n a1 a2
    numPlayers        = length $ allAgents d
    resultLists       = iterSplit numPlayers $! playPair <$> pairsList

getTournPairs :: [a] -> [(a,a)]
getTournPairs = \case
  []    -> []
  [x]   -> []
  x:[y] -> [(x,y)]
  x:xs  -> ((x,) <$> xs) ++ getTournPairs xs

iterSplit :: Int -> [a] -> [[a]]
iterSplit n xs
  | n <= 1    = []
  | otherwise = first : (iterSplit (n-1) rest)
  where
    (first, rest) = splitAt (n-1) xs

scorePlayerN :: [[Value]] -> Int -> Value
scorePlayerN split 0 = case split of
                         []   -> 0
                         x:xs -> sum x
scorePlayerN split n = - (split!!0)!!(n-1) + (scorePlayerN (tail split) (n-1))

playAgents ::
  Int -- board size
  -> (Player -> RState -> Maybe Move) -- agent 1
  -> (Player -> RState -> Maybe Move) -- agent 2
  -> Value -- score for agent 1, 0 means tie, 1 means agent 1 wins both games,
           -- -1 means agent 1 loses both games
playAgents n p1 p2 = (playAgents_ n p1 p2) - (playAgents_ n p2 p1)

playAgents_ ::
  Int -- board size
  -> (Player -> RState -> Maybe Move) -- agent playing Black
  -> (Player -> RState -> Maybe Move) -- agent playing White
  -> Value -- result of the game: 0 = white win, 0.5 = tie, 1 = black win
playAgents_ n pb pw = playAgents__ (pb Black) (pw White) Black $ startBoard n

playAgents__ ::
  (RState -> Maybe Move)    -- agent playing Black
  -> (RState -> Maybe Move) -- agent playing White
  -> Player                 -- player whose turn it is
  -> RState                 -- board state
  -> Value                  -- eventual result
playAgents__ pb pw p s
  | isEndState s           = case getResult s of
      Nothing -> -10 -- should never happen
      Just r  -> endValue r
  | numLegalMoves s p == 0 = playAgents__ pb pw (swapPlayer p) s
  | otherwise              = case currentPlayer s of
      Nothing -> currentLoses
      Just m  -> playAgents__ pb pw (swapPlayer p) (playMove s m)
  where
    (currentPlayer, currentLoses) = case p of
      Black -> (pb, 0.0)
      White -> (pw, 1.0)
    
