import Solver

main = solve part1 part2

part1 = length . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . blink . parse
part2 _ = "N/A"

parse :: String -> [Int]
parse = map read . words

blink :: [Int] -> [Int]
blink ss = foldr morph [] ss

morph :: Int -> [Int] -> [Int]
morph s ss = transform s ++ ss

transform :: Int -> [Int]
transform 0 = [1]
transform s = let s' = show s
                  l = length s'
              in if even l
                 then let (a, b) = splitAt (div l 2) s'
                      in [read a, read b]
                 else [s * 2024]
