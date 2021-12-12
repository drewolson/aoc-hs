module Aoc.Solution.Day12
  ( part1,
    part2,
  )
where

import Data.Char (toLower)
import Data.Foldable (Foldable (foldl'))
import Data.List (nub)
import Data.List.Split (splitOn)
import Data.Map (Map)
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
makeGraph = foldl' addConnection Map.empty
  where
    addEdge :: String -> Maybe [String] -> Maybe [String]
    addEdge node Nothing = Just [node]
    addEdge node (Just nodes) = Just (node : nodes)

    addConnection :: Graph -> (String, String) -> Graph
    addConnection graph (a, b) =
      Map.alter (addEdge a) b $ Map.alter (addEdge b) a graph

isLowercase :: String -> Bool
isLowercase s = s == fmap toLower s

findPaths :: Graph -> Set [String]
findPaths = go False Set.empty "start" []
  where
    addSeen :: Bool -> String -> Set String -> Set String
    addSeen False ('+' : _) seen = seen
    addSeen _ node seen
      | isLowercase node = Set.insert node seen
      | otherwise = seen

    isVisited :: String -> Bool
    isVisited ('+' : _) = True
    isVisited _ = False

    clean :: String -> String
    clean ('+' : s) = s
    clean s = s

    go :: Bool -> Set String -> String -> [String] -> Graph -> Set [String]
    go _ _ "end" path _ = Set.singleton $ clean <$> reverse ("end" : path)
    go visited seen node path graph
      | node `elem` seen = Set.empty
      | otherwise =
        let seen' = addSeen visited node seen
            visited' = visited || isVisited node
            newNodes = Map.findWithDefault [] node graph
            path' = node : path
         in foldMap (\n -> go visited' seen' n path' graph) newNodes

permute :: [(String, String)] -> [[(String, String)]]
permute conns =
  let nodes = nub $ foldMap (\(a, b) -> [a, b]) conns
      small = filter (\n -> n /= "start" && n /= "end") $ filter isLowercase nodes
   in fmap modify small
  where
    modify :: String -> [(String, String)]
    modify target = fmap (replace target) conns

    replace :: String -> (String, String) -> (String, String)
    replace s (a, b)
      | s == a = ("+" <> a, b)
      | s == b = (a, "+" <> b)
      | otherwise = (a, b)

part1 :: String -> Int
part1 = length . findPaths . makeGraph . parseInput

part2 :: String -> Int
part2 = length . foldMap (findPaths . makeGraph) . permute . parseInput
