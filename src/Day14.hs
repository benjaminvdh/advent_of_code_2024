import Solver

main = solve part1 part2

part1 = countQuadrants . update 100 . parse
part2 _ = "N/A"

type Coord = (Int, Int)
type Size = Coord
type Vel = Coord
type Pos = Coord
type Robot = (Pos, Vel)

parse :: String -> (Size, [Robot])
parse = parseSize . map parseRobot . lines
        where parseSize rs = if any (\((x, y), _) -> x >= 11 || y >= 7) rs
                             then ((101, 103), rs)
                             else (( 11,   7), rs)

parseRobot :: String -> Robot
parseRobot l = let l' = drop 2 l
                   sp = takeWhile (/=' ') l'
                   sv = drop (length sp + 3) l'
               in (parseCoord sp, parseCoord sv)

parseCoord :: String -> Coord
parseCoord l = let sx = takeWhile (/=',') l
                   sy = drop (length sx + 1) l
               in (read sx, read sy)

update :: Int -> (Size, [Robot]) -> (Size, [Robot])
update n (s, rs) = (s, map (update' n s) rs)
                   where update' n (sx, sy) ((px, py), (vx, vy)) = let x' = px + n * vx
                                                                       y' = py + n * vy
                                                                   in ((x' `mod` sx, y' `mod` sy), (vx, vy))

countQuadrants :: (Size, [Robot]) -> Int
countQuadrants ((sx, sy), rs) = let mx = sx `div` 2
                                    my = sy `div` 2
                                in countQuadrant       (     0,      0) (mx, my) rs
                                       * countQuadrant (mx + 1,      0) (sx, my) rs
                                       * countQuadrant (     0, my + 1) (mx, sy) rs
                                       * countQuadrant (mx + 1, my + 1) (sx, sy) rs

countQuadrant :: Coord -> Coord -> [Robot] -> Int
countQuadrant (fx, fy) (tx, ty) = length . filter (\((px, py), _) -> fx <= px && px < tx && fy <= py && py < ty)
