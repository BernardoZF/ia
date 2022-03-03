# search.py
# ---------
# Licensing Information:  You are free to use or extend these projects for
# educational purposes provided that (1) you do not distribute or publish
# solutions, (2) you retain this notice, and (3) you provide clear
# attribution to UC Berkeley, including a link to http://ai.berkeley.edu.
# 
# Attribution Information: The Pacman AI projects were developed at UC Berkeley.
# The core projects and autograders were primarily created by John DeNero
# (denero@cs.berkeley.edu) and Dan Klein (klein@cs.berkeley.edu).
# Student side autograding was added by Brad Miller, Nick Hay, and
# Pieter Abbeel (pabbeel@cs.berkeley.edu).


"""
In search.py, you will implement generic search algorithms which are called by
Pacman agents (in searchAgents.py).
"""

from sqlalchemy import false
import util

class SearchProblem:
    """
    This class outlines the structure of a search problem, but doesn't implement
    any of the methods (in object-oriented terminology: an abstract class).

    You do not need to change anything in this class, ever.
    """

    def getStartState(self):
        """
        Returns the start state for the search problem.
        """
        util.raiseNotDefined()

    def isGoalState(self, state):
        """
          state: Search state

        Returns True if and only if the state is a valid goal state.
        """
        util.raiseNotDefined()

    def getSuccessors(self, state):
        """
          state: Search state

        For a given state, this should return a list of triples, (successor,
        action, stepCost), where 'successor' is a successor to the current
        state, 'action' is the action required to get there, and 'stepCost' is
        the incremental cost of expanding to that successor.
        """
        util.raiseNotDefined()

    def getCostOfActions(self, actions):
        """
         actions: A list of actions to take

        This method returns the total cost of a particular sequence of actions.
        The sequence must be composed of legal moves.
        """
        util.raiseNotDefined()


def tinyMazeSearch(problem):
    """
    Returns a sequence of moves that solves tinyMaze.  For any other maze, the
    sequence of moves will be incorrect, so only use this for tinyMaze.
    """
    from game import Directions
    s = Directions.SOUTH
    w = Directions.WEST
    return  [s, s, w, s, w, w, s, w]

def depthFirstSearch(problem):
    """
    Search the deepest nodes in the search tree first.

    Your search algorithm needs to return a list of actions that reaches the
    goal. Make sure to implement a graph search algorithm.

    To get started, you might want to try some of these simple commands to
    understand the search problem that is being passed in:
    """
    "*** YOUR CODE HERE ***"

    start_state = problem.getStartState()
    start_node = (start_state, [])

    open_list = util.Stack()

    visited_list = []

    open_list.push(start_node)

    while not open_list.isEmpty():
        current_state, moves = open_list.pop()
        if current_state not in visited_list:
            visited_list.append(current_state)

            if problem.isGoalState(current_state):
                return moves
            else :
                successors = problem.getSuccessors(current_state)
            
            for successor_state, successor_move, successor_cost in successors:
                new_moves = moves + [successor_move]
                new_node = (successor_state, new_moves)
                open_list.push(new_node)
    empty_moves = []
    return empty_moves

def breadthFirstSearch(problem):
    """Search the shallowest nodes in the search tree first."""
    "*** YOUR CODE HERE ***"
    start_state = problem.getStartState()
    start_node = (start_state, [])

    open_list = util.Queue()

    visited_list = []

    open_list.push(start_node)

    while not open_list.isEmpty():
        current_state, moves = open_list.pop()
        if current_state not in visited_list:
            visited_list.append(current_state)

            if problem.isGoalState(current_state):
                return moves
            else :
                successors = problem.getSuccessors(current_state)
            
            for successor_state, successor_move, successor_cost in successors:
                new_moves = moves + [successor_move]
                new_node = (successor_state, new_moves)
                open_list.push(new_node)
    empty_moves = []
    return empty_moves

def uniformCostSearch(problem):
    """Search the node of least total cost first."""
    "*** YOUR CODE HERE ***"
    start_state = problem.getStartState()
    start_node = (start_state, [], 0)

    open_list = util.PriorityQueue()

    visited_list = []

    open_list.push(start_node, 0)

    while not open_list.isEmpty():
        current_state, moves, cost= open_list.pop()
        if current_state not in visited_list:
            visited_list.append(current_state)

            if problem.isGoalState(current_state):
                return moves
            else :
                successors = problem.getSuccessors(current_state)
            
            for successor_state, successor_move, successor_cost in successors:
                new_moves = moves + [successor_move]
                new_cost = cost + successor_cost
                new_node = (successor_state, new_moves, new_cost)
                open_list.push(new_node, new_cost)
    empty_moves = []
    return empty_moves

def nullHeuristic(state, problem=None):
    """
    A heuristic function estimates the cost from the current state to the nearest
    goal in the provided SearchProblem.  This heuristic is trivial.
    """
    return 0

def aStarSearch(problem, heuristic=nullHeuristic):
    """Search the node that has the lowest combined cost and heuristic first."""
    "*** YOUR CODE HERE ***"
    open_list = util.PriorityQueue()

    visited_list = []

    start_state = problem.getStartState()
    start_node= (start_state, [], 0)

    open_list.push(start_node, 0)

    while not open_list.isEmpty():
        current_state, moves, current_cost = open_list.pop()

        current_node = (current_state, current_cost)
        visited_list.append(current_node)

        if problem.isGoalState(current_state):
            return moves
        else : 
            successors = problem.getSuccessors(current_state)

            for successor_state, successor_moves, successor_cost in successors:
                new_moves = moves + [successor_moves]
                new_cost = problem.getCostOfActions(new_moves)
                new_node = (successor_state, new_moves, new_cost)

                visited_flag = False
                for visited in visited_list:
                    visited_state, visited_cost = visited

                    if (successor_state == visited_state) and (new_cost >= visited_cost):
                        visited_flag = True
                if not visited_flag:
                    open_list.push(new_node, new_cost + heuristic(successor_state, problem))
                    visited_list.append((successor_state, new_cost))

    return []


# Abbreviations
bfs = breadthFirstSearch
dfs = depthFirstSearch
astar = aStarSearch
ucs = uniformCostSearch
