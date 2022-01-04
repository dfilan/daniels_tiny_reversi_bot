Simple reversi bot.

How to play: compile src/Main.hs

TODO: improve heuristic. Ideas:
- compare 'locked tokens', i.e. those tokens that can't be taken from you.
- think about odd vs even number of successive takes across a line.
  - maybe parity until you get a corner?
- focus on things of length >= 3
- in early game, focus on limiting opponent's available moves
- MCTS somehow - perhaps to calibrate above metrics (regress win prob. on them?)

Ways to get even better:
- API for bots to compete against other bots
- Stored games that bots can access to improve heuristic functions
