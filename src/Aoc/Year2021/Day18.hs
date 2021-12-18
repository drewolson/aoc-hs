module Aoc.Year2021.Day18
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, parseInt, runParser')
import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.List (foldl1')
import Text.Megaparsec (sepEndBy1)
import Text.Megaparsec.Char (char, newline)

data SnailFish
  = Reg Int
  | Pair SnailFish SnailFish
  deriving (Eq)

instance Show SnailFish where
  show :: SnailFish -> String
  show = \case
    Reg i -> show i
    Pair l r -> "[" <> show l <> "," <> show r <> "]"

parseReg :: Parser SnailFish
parseReg = Reg <$> parseInt

parsePair :: Parser SnailFish
parsePair = do
  left <- char '[' *> parseSnailFish
  right <- char ',' *> parseSnailFish <* char ']'

  pure $ Pair left right

parseSnailFish :: Parser SnailFish
parseSnailFish = parsePair <|> parseReg

parseAllSnailFish :: Parser [SnailFish]
parseAllSnailFish = sepEndBy1 parseSnailFish newline

parseInput :: String -> [SnailFish]
parseInput = runParser' parseAllSnailFish

explode :: SnailFish -> Maybe SnailFish
explode = fmap fst . go 0
  where
    updateL :: Int -> SnailFish -> SnailFish
    updateL i = \case
      Reg n -> Reg $ n + i
      Pair l r -> Pair (updateL i l) r

    updateR :: Int -> SnailFish -> SnailFish
    updateR i = \case
      Reg n -> Reg $ n + i
      Pair l r -> Pair l (updateR i r)

    go :: Int -> SnailFish -> Maybe (SnailFish, (Int, Int))
    go i = \case
      Reg _ -> Nothing
      Pair (Reg l) (Reg r) | i == 4 -> Just (Reg 0, (l, r))
      Pair l r ->
        case (go (i + 1) l, go (i + 1) r) of
          (Just (l', (lVal, rVal)), _) ->
            Just (Pair l' (updateL rVal r), (lVal, 0))
          (_, Just (r', (lVal, rVal))) ->
            Just (Pair (updateR lVal l) r', (0, rVal))
          _ -> Nothing

split :: SnailFish -> Maybe SnailFish
split = \case
  Reg n | n >= 10 -> Just $ Pair (Reg (n `div` 2)) (Reg ((n + 1) `div` 2))
  Reg _ -> Nothing
  Pair l r ->
    case (split l, split r) of
      (Just l', _) -> Just $ Pair l' r
      (_, Just r') -> Just $ Pair l r'
      _ -> Nothing

reduceSnailFish :: SnailFish -> SnailFish
reduceSnailFish snailFish =
  maybe snailFish reduceSnailFish (explode snailFish <|> split snailFish)

addSnailFish :: SnailFish -> SnailFish -> SnailFish
addSnailFish a b = reduceSnailFish $ Pair a b

magnitude :: SnailFish -> Int
magnitude = \case
  Reg n -> n
  Pair l r -> 3 * magnitude l + 2 * magnitude r

candidates :: [SnailFish] -> [(SnailFish, SnailFish)]
candidates fish = do
  a <- fish
  b <- fish

  guard $ a /= b

  [(a, b), (b, a)]

part1 :: String -> Int
part1 = magnitude . foldl1' addSnailFish . parseInput

part2 :: String -> Int
part2 = maximum . fmap (magnitude . uncurry addSnailFish) . candidates . parseInput
