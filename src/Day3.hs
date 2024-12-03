import Solver

import Data.Char
import Data.List

main = solve part1 part2

part1 input = multAll input
part2 _ = "N/A"

multAll ('m':xs) = parseMul xs + multAll xs
multAll (_:xs) = multAll xs
multAll [] = 0

parseMul :: String -> Int
parseMul xs = if "ul(" `isPrefixOf` xs then
                  let a = takeWhile isDigit (drop 3 xs)
                      b = head (drop (3 + length a) xs)
                      c = takeWhile isDigit (drop (4 + length a) xs)
                      d = drop (4 + length a + length c) xs
                  in if b == ',' && head d == ')' then read a * read c else 0
              else 0
