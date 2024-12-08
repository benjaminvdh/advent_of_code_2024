import Solver

import Data.List
import qualified Data.Map.Strict as M

main = solve part1 part2

part1 input = let (map, size) = parse input
              in length $ nub $ filter (inside size) $ concat $ snd $ unzip $ M.toList $ fmap antinodes map
part2 _ = "N/A"

type Coord = (Int, Int)
type Map = M.Map Char [Coord]

parse :: String -> (Map, (Int, Int))
parse input = let m = foldr (\list map -> foldr addCoords map list) M.empty $ map parseLine $ zip [0..] (lines input)
                  size = getSize input
              in (m, size)

parseLine :: (Int, String) -> [(Char, Coord)]
parseLine (y, l) = map (\(x, c) -> (c, (x, y))) $ filter ((/=)'.' . snd) (zip [0..] l)

addCoords :: (Char, Coord) -> Map -> Map
addCoords (c, p) map = M.alter prependOrUpdate c map
  where prependOrUpdate Nothing = Just [p]
        prependOrUpdate (Just old) = Just (p:old)

getSize :: String -> (Int, Int)
getSize input = let ls = lines input
                in (length $ head ls, length ls)

antinodes :: [Coord] -> [Coord]
antinodes (c:cs) = antinodes' c cs ++ antinodes cs
antinodes [] = []

antinodes' :: Coord -> [Coord] -> [Coord]
antinodes' c (other:cs) = let d = c `subCoord` other
                          in [c `addCoord` d, other `subCoord` d] ++ antinodes' c cs
antinodes' c [] = []

addCoord :: Coord -> Coord -> Coord
(ax, ay) `addCoord` (bx, by) = (ax + bx, ay + by)

subCoord :: Coord -> Coord -> Coord
(ax, ay) `subCoord` (bx, by) = (ax - bx, ay - by)

inside :: (Int, Int) -> Coord -> Bool
inside (w, h) (x, y) = 0 <= x && x < w && 0 <= y && y < h
