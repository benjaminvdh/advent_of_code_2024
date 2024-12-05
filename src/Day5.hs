import Solver

import Data.Char
import Data.List
import Data.Maybe

main = solve part1 part2

part1 input = let (constraints, updates) = parse input
              in (sum (map getMiddle (filter (isOrdered constraints) updates)))
part2 _ = "N/A"

type Constraint = (Int, Int)
type Update = [Int]

parse :: String -> ([Constraint], [Update])
parse input = let l = lines input
                  i = fromJust $ elemIndex [] l
                  (constraints, updates) = splitAt i l
              in (parseConstraints constraints, parseUpdates (tail updates))

parseConstraints :: [String] -> [Constraint]
parseConstraints = map parseConstraint
                   where parseConstraint xs = let a = takeWhile isDigit xs
                                                  b = drop (length a + 1) xs
                                              in (read a, read b)

parseUpdates :: [String] -> [Update]
parseUpdates = map parseUpdate
               where parseUpdate (x:xs) = let num = x:takeWhile isDigit xs
                                          in read num:parseUpdate (drop (length num) xs) 
                     parseUpdate [] = []

isOrdered :: [Constraint] -> Update -> Bool
isOrdered constraints update = all (constraintHolds update) constraints

constraintHolds :: Update -> Constraint -> Bool
constraintHolds update constraint = let (before, after) = constraint
                                        iBefore = elemIndex before update
                                        iAfter = elemIndex after update
                                    in iBefore `isBefore` iAfter

isBefore :: (Ord a) => Maybe a -> Maybe a -> Bool
Just a `isBefore` Just b = a < b
_ `isBefore` _ = True

getMiddle :: [a] -> a
getMiddle l = let len = length l
                  index = find (\i -> i == len - (i + 1)) [0..len]
              in l !! fromJust index
