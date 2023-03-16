module Aoc.Year2021.Day12
  ( part1,
    part2,
  )
where

import Data.Char (toLower)
import Data.Foldable (Foldable (foldl'))
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Graph = Map String [String]

parseInput :: String -> [(String, String)]
parseInput = mapMaybe (toTuple . splitOn "-") . lines
  where
    toTuple :: [a] -> Maybe (a, a)
    toTuple [a, b] = Just (a, b)
    toTuple _ = Nothing

makeGraph :: [(String, String)] -> Graph
makeGraph = foldl' addConnection mempty
  where
    addEdge :: String -> Maybe [String] -> Maybe [String]
    addEdge node Nothing = Just [node]
    addEdge node (Just nodes) = Just (node : nodes)

    addConnection :: Graph -> (String, String) -> Graph
    addConnection graph (a, b) =
      Map.alter (addEdge a) b $ Map.alter (addEdge b) a graph

findPaths :: Bool -> Graph -> Set [String]
findPaths = go mempty "start" []
  where
    isLowercase :: String -> Bool
    isLowercase s = s == fmap toLower s

    addSeen :: String -> Set String -> Set String
    addSeen node seen
      | isLowercase node = Set.insert node seen
      | otherwise = seen

    isSmall :: String -> Bool
    isSmall s
      | s == "start" || s == "end" = False
      | otherwise = isLowercase s

    go :: Set String -> String -> [String] -> Bool -> Graph -> Set [String]
    go seen node path visited graph
      | node == "end" = Set.singleton $ reverse $ "end" : path
      | node `elem` seen = mempty
      | otherwise =
          let seen' = addSeen node seen
              next = graph ! node
              path' = node : path
              paths = foldMap (\n -> go seen' n path' visited graph) next
           in if not visited && isSmall node
                then paths <> foldMap (\n -> go seen n path' True graph) next
                else paths

part1 :: String -> Int
part1 = length . findPaths True . makeGraph . parseInput

part2 :: String -> Int
part2 = length . findPaths False . makeGraph . parseInput
