import Solver

import Data.List

main = solve part1 part2

part1 input = let (g, s, os) = parse input
              in length $ nub $ walk g Up s os

part2 _ = "N/A"

type Coord = (Int, Int)

data Dir = Up | Right | Down | Left deriving (Enum) 

parse :: String -> (Coord, (Int, Int), [Coord])
parse input = let ls = lines input
              in (parseGuard ls, parseSize ls, parseObstacles ls)

parseGuard :: [String] -> Coord
parseGuard input = head $ concat $ map (parseLine '^') $ zip [0..] input

parseObstacles :: [String] -> [Coord]
parseObstacles input = concat $ map (parseLine '#') $ zip [0..] input

parseLine :: Char -> (Int, String) -> [Coord]
parseLine t (y, line) = map (\(x, _) -> (x, y)) $ filter (\(_, c) -> c == t) $ zip [0..] line

parseSize :: [String] -> (Int, Int)
parseSize input = (length (head input), length input)

walk :: Coord -> Dir -> (Int, Int) -> [Coord] -> [Coord]
walk g dir s os 
  | inside s g = let (g', dir') = update g dir os
                 in g:walk g' dir' s os
  | otherwise  = []

inside :: (Int, Int) -> Coord -> Bool
inside (w, h) (x, y) = 0 <= x && x < w && 0 <= y && y < h

update :: Coord -> Dir -> [Coord] -> (Coord, Dir)
update g dir os = let g' = step g dir
                  in if g' `elem` os then let d' = nextDir dir in (step g d', d') else (g', dir)

nextDir Main.Left = Up
nextDir dir       = succ dir

step :: Coord -> Dir -> Coord
step (gx, gy) dir = case dir of
  Up         -> (gx    , gy - 1)
  Main.Right -> (gx + 1, gy    )
  Down       -> (gx    , gy + 1)
  Main.Left  -> (gx - 1, gy    )
