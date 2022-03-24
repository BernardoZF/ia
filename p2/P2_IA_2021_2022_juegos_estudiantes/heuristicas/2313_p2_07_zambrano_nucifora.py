from os import stat
from random import random
from game import (
    TwoPlayerGameState,
)
from tournament import (
    StudentHeuristic,
)


class ANSU_FATI_CHARCUTERO_CORUÑES(StudentHeuristic):
    def get_name(self) -> str:
      return "ANSU FATI CHARCUTERO CORUÑES"

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


class NFT_a_precio_de_floor(StudentHeuristic):
  def get_name(self) -> str:
      return "NFT_a_precio_de_floor"

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
        return random()

          


class Gormiti_de_Willyrex(StudentHeuristic):
  def get_name(self) -> str:
      return "Gormiti_de_Willyrex"

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
          return random()
      else : 
        return random()
