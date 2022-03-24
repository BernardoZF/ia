# pylint: disable=missing-class-docstring
"""Illustration of tournament.

Authors:
    Alejandro Bellogin <alejandro.bellogin@uam.es>

"""

from __future__ import annotations  # For Python 3.7

import numpy as np

from game import Player, TwoPlayerGameState, TwoPlayerMatch
from heuristic import simple_evaluation_function
from reversi import (
    Reversi,
    from_array_to_dictionary_board,
    from_dictionary_to_array_board,
)
from tournament import StudentHeuristic, Tournament


class Heuristic1(StudentHeuristic):

    def get_name(self) -> str:
        return "dummy"

    def evaluation_function(self, state: TwoPlayerGameState) -> float:
        # Use an auxiliary function.
        return self.dummy(123)

    def dummy(self, n: int) -> int:
        return n + 4


class Heuristic2(StudentHeuristic):

    def get_name(self) -> str:
        return "random"

    def evaluation_function(self, state: TwoPlayerGameState) -> float:
        return float(np.random.rand())


class Heuristic3(StudentHeuristic):

    def get_name(self) -> str:
        return "heuristic"

    def evaluation_function(self, state: TwoPlayerGameState) -> float:
        return simple_evaluation_function(state)


class ANSU_FATI_CHARCUTERO_CORUÑES(StudentHeuristic):
  def get_name(self) -> str:
      return "ANSU FATI CHARCUTERO CORUÑES"

  def evaluation_function(self, state: TwoPlayerGameState) -> float:
      corner = [state.board.get((1,1)), state.board.get((1, state.game.height)), state.board.get((state.game.width, 1)), state.board.get((state.game.width, state.game.height))]
      corner_diff = 0

      # Se comprueba si se pasa en alguna esquina
      for i in range (4):
        if corner[i] == state.game.player1.label:
          corner_diff += 10
        if corner[i] == state.game.player2.label:
          corner_diff -= 10
      #Comprobamos de quien es el turno
      if state.is_player_max(state.player1):
        move_player1 = len(state.game._get_valid_moves(state.board, state.player1.label))
        diff = 4 * corner_diff - move_player1
        # Si es del player 1 devolvemos la diferencia normal
        return diff
      if state.is_player_max(state.player2):
        move_player2 = len(state.game._get_valid_moves(state.board, state.player2.label))
        diff = 4 * corner_diff - move_player2
        # Si es del player 2 devolvemos la diferencia en negativo
        return  - diff


def create_match(player1: Player, player2: Player) -> TwoPlayerMatch:

    initial_board = None#np.zeros((dim_board, dim_board))
    initial_player = player1

    """game = TicTacToe(
        player1=player1,
        player2=player2,
        dim_board=dim_board,
    )"""

    """
    initial_board = (
        ['..B.B..',
        '.WBBW..',
        'WBWBB..',
        '.W.WWW.',
        '.BBWBWB']
    )
    """

    if initial_board is None:
        height, width = 8, 8
    else:
        height = len(initial_board)
        width = len(initial_board[0])
        try:
            initial_board = from_array_to_dictionary_board(initial_board)
        except ValueError:
            raise ValueError('Wrong configuration of the board')
        else:
            print("Successfully initialised board from array")

    game = Reversi(
        player1=player1,
        player2=player2,
        height=8,
        width=8
    )

    game_state = TwoPlayerGameState(
        game=game,
        board=initial_board,
        initial_player=initial_player,
    )

    return TwoPlayerMatch(game_state, max_seconds_per_move=1000, gui=True)


tour = Tournament(max_depth=4, init_match=create_match)
strats = {
    'opt1': [Heuristic1],
    'opt2': [Heuristic3],
    'opt3': [ANSU_FATI_CHARCUTERO_CORUÑES]
    }

n = 1
scores, totals, names = tour.run(
    student_strategies=strats,
    increasing_depth=False,
    n_pairs=n,
    allow_selfmatch=False,
)

print(
    'Results for tournament where each game is repeated '
    + '%d=%dx2 times, alternating colors for each player' % (2 * n, n),
)

# print(totals)
# print(scores)

print('\ttotal:', end='')
for name1 in names:
    print('\t%s' % (name1), end='')
print()
for name1 in names:
    print('%s\t%d:' % (name1, totals[name1]), end='')
    for name2 in names:
        if name1 == name2:
            print('\t---', end='')
        else:
            print('\t%d' % (scores[name1][name2]), end='')
    print()
