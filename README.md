Simple reversi bot.

How to play: execute `cabal run`

Biggest TODO: figure out why code is so slow

TODO: improve heuristic. Ideas:
- think about odd vs even number of successive takes across a line.
  - maybe parity until you get a corner?
- focus on things of length >= 3
- in early game, focus on limiting opponent's available moves
- MCTS somehow - perhaps to calibrate above metrics (regress win prob. on them?)

Ways to get even better:
- Get stats for how much of tree is being pruned
- Stored games that bots can access to improve heuristic functions

Housekeeping:
- rename Agent.hs to MinimaxAgent.hs
- move heuristics to their own file?
