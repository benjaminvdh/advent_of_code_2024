import Solver

import Data.Char
import Data.List

main = solve part1 part2

part1 = sum . map countMatches . concat . morph . lines
part2 input = "N/A"

morph :: [String] -> [[String]]
morph input = let vrev = transpose input 
                  hrev = map reverse input
                  vhrev = map reverse vrev
                  diag = diagonaliseBlock input
                  vdiag = diagonaliseBlock $ reverse hrev
                  hdiag = diagonaliseBlock hrev
                  vhdiag = diagonaliseBlock vhrev
              in [input, hrev, vrev, vhrev, diag, vdiag, hdiag, vhdiag]

diagonaliseBlock ss = diagonaliseRows ss ++ diagonaliseRows (transpose (map tail ss))

diagonaliseRows :: [String] -> [String]
diagonaliseRows (l:ls) = diagonaliseOnce (l:ls):diagonaliseRows ls
diagonaliseRows [] = []

diagonaliseOnce :: [String] -> String
diagonaliseOnce ((l:_):lss) = l:diagonaliseOnce (map tail lss)
diagonaliseOnce _ = []

countMatches :: String -> Int
countMatches a@(_:ss) = countMatches ss + if "XMAS" `isPrefixOf` a then 1 else 0
countMatches [] = 0
