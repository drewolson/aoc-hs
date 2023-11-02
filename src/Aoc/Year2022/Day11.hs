module Aoc.Year2022.Day11
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Data.List (foldl', partition, sort)
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Text.Megaparsec (choice, sepBy1)
import Text.Megaparsec.Char (char, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Monkey = Monkey
  { num :: Int,
    items :: [Integer],
    op :: Integer -> Integer,
    divTest :: Integer,
    ifTrue :: Int,
    ifFalse :: Int
  }

type State = Map Int Monkey

type Counts = Map Int Integer

opP :: Parser (Integer -> Integer)
opP =
  choice
    [ (^ (2 :: Integer)) <$ string "new = old * old",
      (+) <$> (string "new = old + " *> decimal),
      (*) <$> (string "new = old * " *> decimal)
    ]

monkeyP :: Parser Monkey
monkeyP =
  Monkey
    <$> (string "Monkey " *> decimal <* char ':' <* newline)
    <*> (string "  Starting items: " *> sepBy1 decimal (string ", ") <* newline)
    <*> (string "  Operation: " *> opP <* newline)
    <*> (string "  Test: divisible by " *> decimal <* newline)
    <*> (string "    If true: throw to monkey " *> decimal <* newline)
    <*> (string "    If false: throw to monkey " *> decimal <* newline)

monkeysP :: Parser [Monkey]
monkeysP = sepBy1 monkeyP newline

parseInput :: String -> State
parseInput = runParser $ do
  monkeys <- monkeysP

  pure $ Map.fromList $ (\m -> (num m, m)) <$> monkeys

playRound :: Bool -> (Counts, State) -> (Counts, State)
playRound tooBig acc = foldl' updateMonkey acc [0 .. length (snd acc) - 1]
  where
    incCount :: Int -> Maybe Integer -> Maybe Integer
    incCount n Nothing = Just $ fromIntegral n
    incCount n (Just m) = Just $ fromIntegral n + m

    addItems :: [Integer] -> Monkey -> Monkey
    addItems new m@Monkey {items} = m {items = items <> new}

    clearItems :: Monkey -> Monkey
    clearItems m = m {items = []}

    reduce :: Integer -> Integer
    reduce n =
      if tooBig
        then n `mod` product (divTest <$> snd acc)
        else n `div` 3

    updateMonkey :: (Counts, State) -> Int -> (Counts, State)
    updateMonkey (counts, state) n =
      let Monkey {num, items, op, divTest, ifTrue, ifFalse} = state ! n
          counts' = Map.alter (incCount $ length items) num counts
          items' = reduce . op <$> items
          (trueItems, falseItems) = partition ((== 0) . (`mod` divTest)) items'
          state' =
            Map.adjust clearItems num $
              Map.adjust (addItems trueItems) ifTrue $
                Map.adjust (addItems falseItems) ifFalse state
       in (counts', state')

part1 :: String -> Integer
part1 =
  product
    . take 2
    . reverse
    . sort
    . Map.elems
    . fst
    . (!! 20)
    . iterate (playRound False)
    . (Map.empty,)
    . parseInput

part2 :: String -> Integer
part2 =
  product
    . take 2
    . reverse
    . sort
    . Map.elems
    . fst
    . (!! 10000)
    . iterate (playRound True)
    . (Map.empty,)
    . parseInput
