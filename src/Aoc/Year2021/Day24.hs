module Aoc.Year2021.Day24
  ( part1,
  )
where

import Aoc.Parser (Parser, parseSignedInt, runParser')
import Control.Applicative ((<|>))
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.Map.Strict (Map, (!))
import Data.Map.Strict qualified as Map
import Text.Megaparsec (choice, sepEndBy1, some)
import Text.Megaparsec.Char (letterChar, newline, space1, string)

type Var = String

type Arg = Either Var Int

data Expr
  = Inp Var
  | Add Var Arg
  | Mul Var Arg
  | Div Var Arg
  | Mod Var Arg
  | Eql Var Arg
  deriving (Show)

type State = (Map String Int, [Int])

parseVar :: Parser Var
parseVar = some letterChar

parseArg :: Parser Arg
parseArg = Right <$> parseSignedInt <|> Left <$> parseVar

parseOp :: (Var -> Arg -> Expr) -> String -> Parser Expr
parseOp con name =
  con <$> (string name *> space1 *> parseVar) <*> (space1 *> parseArg)

parseExpr :: Parser Expr
parseExpr =
  choice
    [ Inp <$> (string "inp " *> parseVar),
      parseOp Add "add",
      parseOp Mul "mul",
      parseOp Div "div",
      parseOp Mod "mod",
      parseOp Eql "eql"
    ]

parseExprs :: Parser [Expr]
parseExprs = sepEndBy1 parseExpr newline

parseInput :: String -> [Expr]
parseInput = runParser' parseExprs

intToDigits :: Int -> [Int]
intToDigits = map digitToInt . show

digitsToInt :: [Int] -> Int
digitsToInt = read . foldMap show

nonZeros :: [Int] -> Bool
nonZeros = notElem 0

candidates :: [[Int]]
candidates =
  filter nonZeros $ fmap intToDigits [99999999999999, 99999999999998 .. 11111111111111]

runProgram :: [Expr] -> [Int] -> Map String Int
runProgram e i = fst $ foldl' runExpr (initialState, i) e
  where
    initialState :: Map String Int
    initialState =
      Map.fromList
        [ ("w", 0),
          ("x", 0),
          ("y", 0),
          ("z", 0)
        ]

    evalArg :: Arg -> Map String Int -> Int
    evalArg (Right a) _ = a
    evalArg (Left v) env = env ! v

    alterVar :: (Int -> Int -> Int) -> Var -> Arg -> State -> State
    alterVar f var arg (env, input) =
      let a = env ! var
          b = evalArg arg env
       in (Map.insert var (f a b) env, input)

    eql :: Int -> Int -> Int
    eql a b =
      if a == b
        then 1
        else 0

    runExpr :: State -> Expr -> State
    runExpr state@(env, input) = \case
      Inp var -> (Map.insert var (head input) env, tail input)
      Add var arg -> alterVar (+) var arg state
      Mul var arg -> alterVar (*) var arg state
      Div var arg -> alterVar div var arg state
      Mod var arg -> alterVar mod var arg state
      Eql var arg -> alterVar eql var arg state

validState :: Map String Int -> Bool
validState = (== 0) . (! "z")

part1 :: String -> Int
part1 input =
  let instructions = parseInput input
   in digitsToInt $ head $ filter (validState . runProgram instructions) candidates
