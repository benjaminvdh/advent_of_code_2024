import Solver

import Data.Char
import Data.List
import Data.Maybe

main = solve part1 part2

part1 = checksum . compact . parse
part2 _ = "N/A"

parse :: String -> [Maybe Int]
parse input = let (_, list, _) = foldl expand (True, [], 0) input 
              in reverse list

expand :: (Bool, [Maybe Int], Int) -> Char -> (Bool, [Maybe Int], Int)
expand acc '\n'           = acc
expand  (True, acc, id) c = let n = digitToInt c
                            in (False, replicate n (Just id) ++ acc, id + 1)
expand (False, acc, id) c = let n = digitToInt c
                            in (True, replicate n Nothing ++ acc, id)

compact :: [Maybe Int] -> [Int]
compact ds = let n = length (filter isJust ds)
                 c = compact' ds (reverse ds)
             in take n c

compact' :: [Maybe Int] -> [Maybe Int] -> [Int]
compact'  (Just d:ds) rs = d:compact' ds rs
compact' (Nothing:ds) rs = let rs' = dropWhile isNothing rs
                           in case uncons rs' of
                                   Just (Just d, rest) -> d:compact' ds rest
                                   _                   -> []
compact' [] _ = []

checksum :: [Int] -> Int
checksum = sum . map (uncurry (*)) . zip [0..]
