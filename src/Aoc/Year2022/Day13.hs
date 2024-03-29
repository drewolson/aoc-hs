module Aoc.Year2022.Day13
  ( part1,
    part2,
  )
where

import Aoc.Core.Parser (Parser, runParser)
import Data.List (elemIndex, sort)
import Text.Megaparsec (between, sepBy, sepEndBy1, (<|>))
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)

data Packet
  = L [Packet]
  | V Int
  deriving (Eq)

instance Ord Packet where
  compare :: Packet -> Packet -> Ordering
  compare (V l) (V r) = compare l r
  compare (L l) (L r) = compare l r
  compare (L l) v = compare l [v]
  compare v (L r) = compare [v] r

div1 :: Packet
div1 = L [L [V 2]]

div2 :: Packet
div2 = L [L [V 6]]

parseInput :: String -> [(Packet, Packet)]
parseInput = runParser $ sepEndBy1 packetPairP newline
  where
    listP :: Parser Packet
    listP = L <$> between (char '[') (char ']') (sepBy packetP (char ','))

    valueP :: Parser Packet
    valueP = V <$> decimal

    packetP :: Parser Packet
    packetP = listP <|> valueP

    packetPairP :: Parser (Packet, Packet)
    packetPairP = (,) <$> (packetP <* newline) <*> (packetP <* newline)

collapse :: [(a, a)] -> [a]
collapse = concatMap (\(a, b) -> [a, b])

part1 :: String -> Int
part1 = sum . fmap fst . filter (uncurry (<) . snd) . zip [1 ..] . parseInput

part2 :: String -> Maybe Int
part2 input = do
  let packets = sort $ collapse (parseInput input) <> [div1, div2]
  i1 <- elemIndex div1 packets
  i2 <- elemIndex div2 packets

  pure $ (i1 + 1) * (i2 + 1)
