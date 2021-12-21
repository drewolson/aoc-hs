module Aoc.Year2021.Day21
  ( part1,
    part2,
  )
where

import Control.Monad.ST (ST, runST)
import Data.Bifunctor (Bifunctor (bimap))
import Data.Foldable (maximumBy)
import Data.HashTable.ST.Cuckoo (HashTable)
import Data.HashTable.ST.Cuckoo qualified as HashTable
import Data.Hashable (Hashable)
import Data.List.Split (divvy)
import Data.Ord (comparing)
import GHC.Generics (Generic)

data Player = Player
  { name :: Int,
    pos :: Int,
    score :: Int
  }
  deriving (Eq, Generic, Hashable)

type State = (Player, Player)

type Cache s = HashTable s State (Int, Int)

move :: Int -> Int -> Int
move cur next = ((cur - 1 + next) `mod` 10) + 1

addScore :: State -> Int -> State
addScore (p@Player {pos, score}, next) n =
  let pos' = move pos n
   in (next, p {pos = pos', score = score + pos'})

isWinner :: Int -> State -> Bool
isWinner n (a, b) = score a >= n || score b >= n

result :: (Int, State) -> Int
result (turn, (a, b)) = (turn * 3) * minimum (fmap score [a, b])

winTuple :: State -> (Int, Int)
winTuple (a, b) =
  case maximumBy (comparing score) [a, b] of
    Player {name = 1} -> (1, 0)
    _ -> (0, 1)

scoreAll :: Cache s -> State -> ST s (Int, Int)
scoreAll cache state
  | isWinner 21 state = pure $ winTuple state
  | otherwise = do
    prev <- HashTable.lookup cache state

    case prev of
      Just v -> pure v
      Nothing -> do
        results <- traverse (scoreAll cache) do
          a <- [1 .. 3]
          b <- [1 .. 3]
          c <- [1 .. 3]

          pure $ addScore state (a + b + c)

        let v = bimap sum sum $ unzip results

        HashTable.insert cache state v

        pure v

part1 :: Int -> Int -> Int
part1 a b =
  let playerA = Player {name = 1, pos = a, score = 0}
      playerB = Player {name = 2, pos = b, score = 0}
      states = scanl addScore (playerA, playerB) $ fmap sum $ divvy 3 3 $ cycle [1 .. 100]
   in result $ head $ dropWhile (not . isWinner 1000 . snd) $ zip [0 ..] states

part2 :: Int -> Int -> Int
part2 a b =
  let playerA = Player {name = 1, pos = a, score = 0}
      playerB = Player {name = 2, pos = b, score = 0}
   in uncurry max $ runST do
        cache <- HashTable.new
        scoreAll cache (playerA, playerB)
