import numpy as np

from game import Player, TwoPlayerGameState, TwoPlayerMatch
from heuristic import simple_evaluation_function
from tictactoe import TicTacToe
from tournament import StudentHeuristic, Tournament
from reversi import Reversi
import timeit


class Heuristic1(StudentHeuristic):

    def get_name(self) -> str:
        return "ANSU"

    def evaluation_function(self, state: TwoPlayerGameState) -> float:
        corner = [
            state.board.get((1, 1)),
            state.board.get((1, state.game.height)),
            state.board.get((state.game.width, 1)),
            state.board.get((state.game.width, state.game.height))
            ]
        corner_diff = 0

        # Se comprueba si se pasa en alguna esquina
        for i in range(4):
          if corner[i] == state.game.player1.label:
            corner_diff += 10
          if corner[i] == state.game.player2.label:
            corner_diff -= 10
          # Comprobamos de quien es el turno
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


class Heuristic2(StudentHeuristic):

    def get_name(self) -> str:
        return "PEPE"
    def evaluation_function(self, state: TwoPlayerGameState) -> float:
      corner = [state.board.get((1,1)), state.board.get((1, state.game.height)), state.board.get((state.game.width, 1)), state.board.get((state.game.width, state.game.height))]

      corners_p1 = corner.count(state.player1.label)
      corners_p2 = corner.count(state.player2.label)

      move_player1 = len(state.game._get_valid_moves(state.board, state.player1.label))
      move_player2 = len(state.game._get_valid_moves(state.board, state.player2.label))

      if((move_player1 - move_player2) != 0):
        if(corners_p1 - corners_p2) != 0:
          return 0.3 * (100 * (move_player1 - move_player2)/(move_player1 + move_player2)) + 0.4 * (100 * (corners_p1 - corners_p2) / (corners_p1 + corners_p2)) + 0.3 *(state.game._choice_diff(state.board))
        else:
          return 0
      else : 
        return 0


class Heuristic3(StudentHeuristic):

    def get_name(self) -> str:
        return "BOB"

    def evaluation_function(self, state: TwoPlayerGameState) -> float:
      corner = [state.board.get((1,1)), state.board.get((1, state.game.height)), state.board.get((state.game.width, 1)), state.board.get((state.game.width, state.game.height))]

      corners_p1 = corner.count(state.player1.label)
      corners_p2 = corner.count(state.player2.label)

      move_player1 = len(state.game._get_valid_moves(state.board, state.player1.label))
      move_player2 = len(state.game._get_valid_moves(state.board, state.player2.label))

      if((move_player1 - move_player2) != 0):
        if(corners_p1 - corners_p2) != 0:
          return 0.3 * (100 * (move_player1 - move_player2)/(move_player1 + move_player2)) + 0.4 * (100 * (corners_p1 - corners_p2) / (corners_p1 + corners_p2)) + 0.3 *(state.game._choice_diff(state.board))
        else:
          return 0.5 * (100 * (move_player1 - move_player2)/(move_player1 + move_player2)) +  0.5 *(state.game._choice_diff(state.board))
      else : 
        return 0

def create_match(player1: Player, player2: Player) -> TwoPlayerMatch:
    height = 4
    width = 4

    initial_board = None
    initial_player = player1

    game = Reversi(
        player1=player1,
        player2=player2,
        height=height,
        width=width,
    )

    game_state = TwoPlayerGameState(
        game=game,
        board=initial_board,
        initial_player=initial_player,
    )

    return TwoPlayerMatch(game_state, gui=False)

def init_match():
    tour = Tournament(max_depth=4, init_match=create_match)
    strats = {'opt1': [Heuristic1], 'opt2': [Heuristic2], 'opt3': [Heuristic3]}

    n = 5
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

print("minimax vs minimax heuristic prof=4 %f" % (timeit.timeit(init_match, number=1)))