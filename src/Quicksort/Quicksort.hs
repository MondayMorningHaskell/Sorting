module Quicksort where

-- Simple, recursive definition of Quicksort,
-- using O(n) memory (not in-place)

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

unsorted :: [Int]
unsorted = [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

sorted :: [Int]
sorted = quicksort unsorted

run = print sorted
