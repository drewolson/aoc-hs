module Aoc.Year2021.Day23
  ( sample,
    part1,
  )
where

import Algorithm.Search (dijkstra)
import Data.Set (Set, (\\))
import Data.Set qualified as Set

type Coord = (Int, Int)

type Board = Set Coord

type Player = ((Char, Int), Coord)

type State = Set Player

board :: Board
board =
  Set.fromList
    [ (0, 0),
      (1, 0),
      (2, 0),
      (3, 0),
      (4, 0),
      (5, 0),
      (6, 0),
      (7, 0),
      (8, 0),
      (9, 0),
      (10, 0),
      (2, 1),
      (2, 2),
      (4, 1),
      (4, 2),
      (6, 1),
      (6, 2),
      (8, 1),
      (8, 2)
    ]

sample :: State
sample =
  Set.fromList
    [ (('B', 1), (2, 1)),
      (('A', 1), (2, 2)),
      (('C', 1), (4, 1)),
      (('D', 1), (4, 2)),
      (('B', 2), (6, 1)),
      (('C', 2), (6, 2)),
      (('D', 2), (8, 1)),
      (('A', 2), (8, 2))
    ]

playerCost :: Char -> Int
playerCost = \case
  'A' -> 1
  'B' -> 10
  'C' -> 100
  _ -> 1000

isSolved :: Player -> Bool
isSolved ((p, _), (x, y))
  | p == 'A' = x == 2 && y > 0
  | p == 'B' = x == 4 && y > 0
  | p == 'C' = x == 6 && y > 0
  | otherwise = x == 8 && y > 0

candidates :: Player -> [Player]
candidates (p, (x, y)) =
  [ (p, (x + 1, y)),
    (p, (x - 1, y)),
    (p, (x, y + 1)),
    (p, (x, y - 1))
  ]

makeMove :: State -> Player -> Player -> State
makeMove state old new = Set.insert new $ Set.delete old state

makeMoves :: State -> Board -> Player -> [State]
makeMoves state occupied player =
  fmap (makeMove state player) $ filter ((`notElem` occupied) . snd) $ filter ((`elem` board) . snd) $ candidates player

nextStates :: State -> [State]
nextStates state =
  let occupied = Set.map snd state
   in foldMap (makeMoves state occupied) state

cost :: State -> State -> Int
cost cur next =
  sum $ Set.map (playerCost . fst . fst) $ cur \\ next

isGoal :: State -> Bool
isGoal = all isSolved

part1 :: State -> Maybe Int
part1 = fmap fst . dijkstra nextStates cost isGoal
