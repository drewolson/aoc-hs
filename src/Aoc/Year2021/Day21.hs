module Aoc.Year2021.Day21
  ( part1,
    part2,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Foldable (maximumBy)
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.HashTable.ST.Cuckoo qualified as HashTable
import Data.List.Split (divvy)
import Data.Ord (comparing)

data Player = Player
  { name :: Int,
    pos :: Int,
    score :: Int
  }

type Key = ((Int, Int, Int), (Int, Int, Int))

type Cache s = HashTable s Key (Int, Int)

move :: Int -> Int -> Int
move cur next = (((cur - 1) + next) `mod` 10) + 1

addScore :: (Player, Player) -> Int -> (Player, Player)
addScore (Player {name, pos, score}, next) n =
  let pos' = move pos n
   in (next, Player {name, pos = pos', score = score + pos'})

isWinner :: Int -> (Player, Player) -> Bool
isWinner n (a, b) = score a >= n || score b >= n

result :: (Int, (Player, Player)) -> Int
result (turn, (a, b)) = (turn * 3) * minimum (fmap score [a, b])

winTuple :: (Player, Player) -> (Int, Int)
winTuple (a, b) =
  case maximumBy (comparing score) [a, b] of
    Player {name = 1} -> (1, 0)
    _ -> (0, 1)

toHashKey :: (Player, Player) -> Key
toHashKey (a, b) = ((name a, pos a, score a), (name b, pos b, score b))

scoreAll :: Cache s -> (Player, Player) -> ST s (Int, Int)
scoreAll cache p
  | isWinner 21 p = pure $ winTuple p
  | otherwise = do
    prev <- HashTable.lookup cache $ toHashKey p

    case prev of
      Just v -> pure v
      Nothing -> do
        results <- traverse (scoreAll cache) do
          a <- [1 .. 3]
          b <- [1 .. 3]
          c <- [1 .. 3]

          pure $ addScore p (a + b + c)

        let v = (sum $ fmap fst results, sum $ fmap snd results)

        HashTable.insert cache (toHashKey p) v

        pure v

part1 :: Int -> Int -> Int
part1 a b =
  let playerA = Player {name = 1, pos = a, score = 0}
      playerB = Player {name = 2, pos = b, score = 0}
   in result $ head $ dropWhile (not . isWinner 1000 . snd) $ zip [0 ..] $ scanl addScore (playerA, playerB) $ fmap sum $ divvy 3 3 $ cycle [1 .. 100]

part2 :: Int -> Int -> Int
part2 a b =
  let playerA = Player {name = 1, pos = a, score = 0}
      playerB = Player {name = 2, pos = b, score = 0}
      (scoreA, scoreB) = runST do
        cache <- HashTable.new
        scoreAll cache (playerA, playerB)
   in max scoreA scoreB
