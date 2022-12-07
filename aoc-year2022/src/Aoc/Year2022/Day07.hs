module Aoc.Year2022.Day07 where

import Aoc.Parser (Parser, runParser')
import Control.Applicative ((<|>))
import Data.Monoid (Sum (..))
import Text.Megaparsec (choice, sepEndBy1, some)
import Text.Megaparsec.Char (char, letterChar, newline, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Cmd
  = Cd String
  | Ls
  | DirCmd String
  | FileCmd Int String
  deriving (Show)

data Fs
  = File String Int
  | Dir String [Fs]
  deriving (Show)

nameP :: Parser String
nameP = some (letterChar <|> char '/' <|> char '.')

cmdP :: Parser Cmd
cmdP =
  choice
    [ Cd <$> (string "$ cd " *> nameP),
      Ls <$ string "$ ls",
      DirCmd <$> (string "dir " *> nameP),
      FileCmd <$> decimal <*> (char ' ' *> nameP)
    ]

parseInput :: String -> [Cmd]
parseInput = runParser' $ sepEndBy1 cmdP newline

buildFs :: [Cmd] -> Fs
buildFs = fst . buildDir "/" . drop 2
  where
    buildNodes :: [Fs] -> [Cmd] -> ([Fs], [Cmd])
    buildNodes nodes (DirCmd _ : t) = buildNodes nodes t
    buildNodes nodes (Cd ".." : t) = (nodes, t)
    buildNodes nodes (FileCmd i s : t) =
      buildNodes (File s i : nodes) t
    buildNodes nodes (Cd dir : t) =
      let (node, cmds') = buildDir dir (drop 1 t)
       in buildNodes (node : nodes) cmds'
    buildNodes nodes _ = (nodes, [])

    buildDir :: String -> [Cmd] -> (Fs, [Cmd])
    buildDir name cmds =
      let (nodes, cmds') = buildNodes [] cmds
       in (Dir name nodes, cmds')

dirSizes :: Fs -> [Int]
dirSizes = fmap getSum . snd . go
  where
    go :: Fs -> (Sum Int, [Sum Int])
    go (File _ i) = (Sum i, [])
    go (Dir _ fs) =
      let (n, ns) = foldMap go fs
       in (n, n : ns)

part1 :: String -> Int
part1 = sum . filter (<= 100000) . dirSizes . buildFs . parseInput

part2 :: String -> Int
part2 input =
  case dirSizes . buildFs . parseInput $ input of
    [] -> 0
    (h : t) ->
      let goal = 30000000 - (70000000 - h)
       in minimum $ filter (>= goal) t
