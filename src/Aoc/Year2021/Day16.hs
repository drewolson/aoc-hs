module Aoc.Year2021.Day16
  ( part1,
    part2,
  )
where

import Aoc.Parser (Parser, runParser')
import Data.Char (digitToInt)
import Data.Foldable (Foldable (foldl'))
import Text.Megaparsec (count, getOffset, manyTill, manyTill_)
import Text.Megaparsec.Char (char, digitChar)

data Expr
  = Lit Int Int Int
  | Op Int Int [Expr]

expand :: String -> String
expand = foldMap expandChar
  where
    expandChar :: Char -> String
    expandChar = \case
      '0' -> "0000"
      '1' -> "0001"
      '2' -> "0010"
      '3' -> "0011"
      '4' -> "0100"
      '5' -> "0101"
      '6' -> "0110"
      '7' -> "0111"
      '8' -> "1000"
      '9' -> "1001"
      'A' -> "1010"
      'B' -> "1011"
      'C' -> "1100"
      'D' -> "1101"
      'E' -> "1110"
      _ -> "1111"

binToInt :: String -> Int
binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

parse3DigitBin :: Parser Int
parse3DigitBin = binToInt <$> count 3 digitChar

litGroup :: Parser String
litGroup = char '1' *> count 4 digitChar

lastLitGroup :: Parser String
lastLitGroup = char '0' *> count 4 digitChar

parseLiteral :: Parser Int
parseLiteral = do
  (groups, end) <- manyTill_ litGroup lastLitGroup

  pure $ binToInt $ mconcat $ groups <> [end]

offsetIs :: Int -> Parser ()
offsetIs n = do
  offset <- getOffset

  if n == offset
    then pure ()
    else fail "not at offset yet"

parseLengthOp :: Parser [Expr]
parseLengthOp = do
  childLength <- binToInt <$> count 15 digitChar
  currentOffset <- getOffset

  manyTill parseExpr (offsetIs $ currentOffset + childLength)

parseCountOp :: Parser [Expr]
parseCountOp = do
  childNum <- binToInt <$> count 11 digitChar

  count childNum parseExpr

parseOp :: Parser [Expr]
parseOp = do
  opId <- digitChar

  case opId of
    '0' -> parseLengthOp
    _ -> parseCountOp

parseExpr :: Parser Expr
parseExpr = do
  v <- parse3DigitBin
  exprId <- parse3DigitBin

  case exprId of
    4 -> Lit v exprId <$> parseLiteral
    _ -> Op v exprId <$> parseOp

sumVersions :: Expr -> Int
sumVersions = \case
  Lit v _ _ -> v
  Op v _ children -> v + sum (fmap sumVersions children)

eval :: Expr -> Int
eval = \case
  Lit _ _ v -> v
  Op _ 0 children -> sum $ fmap eval children
  Op _ 1 children -> product $ fmap eval children
  Op _ 2 children -> minimum $ fmap eval children
  Op _ 3 children -> maximum $ fmap eval children
  Op _ 5 [l, r] | eval l > eval r -> 1
  Op _ 6 [l, r] | eval l < eval r -> 1
  Op _ 7 [l, r] | eval l == eval r -> 1
  _ -> 0

part1 :: String -> Int
part1 = sumVersions . runParser' parseExpr . expand . init

part2 :: String -> Int
part2 = eval . runParser' parseExpr . expand . init
