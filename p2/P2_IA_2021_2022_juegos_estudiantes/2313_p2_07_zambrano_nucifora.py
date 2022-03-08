from os import stat
from game import (
    TwoPlayerGameState,
)
from tournament import (
    StudentHeuristic,
)


class Heuristic1_Nucifora_Zambrano(StudentHeuristic):
  def get_name(self) -> str:
      return "Heuristic1_Nucifora_Zambrano"

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
        diff = 3 * corner_diff - move_player1
        # Si es del player 1 devolvemos la diferencia normal
        return diff
      if state.is_player_max(state.player2):
        move_player2 = len(state.game._get_valid_moves(state.board, state.player2.label))
        diff = 3 * corner_diff - move_player2
        # Si es del player 2 devolvemos la diferencia en negativo
        return  - diff
