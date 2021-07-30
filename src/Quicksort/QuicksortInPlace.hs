module QuicksortInPlace where

-- Enhanced "in-place" definition of Quicksort.
-- The 'quicksort' function by itself allocates O(1) memory.

-- Note though that 'quicksortPure' must copy its input (via 'thaw')
-- before sorting, and hence takes O(n) memory.

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Ix

swap :: (Num i, Ix i, MArray a e m) => a i e -> i -> i -> m ()
swap arr i j = do
  elem1 <- readArray arr i
  elem2 <- readArray arr j
  writeArray arr i elem2
  writeArray arr j elem1

-- Inner part of the 'for loop' for partition step.
-- If the element at 'i' is smaller than the pivot element,
-- swap it into the smaller/left portion of the array, and
-- advance the pivot index.
partitionLoop :: (Ord e, Num i, Ix i, MArray a e m)
  => a i e -> e -> i -> StateT i m ()
partitionLoop arr pivotElement i = do
  pivotIndex <- get
  currentElement <- lift $ readArray arr i
  when (currentElement <= pivotElement) $ do
    -- Swap smaller element into current pivot index
    lift $ swap arr i pivotIndex
    -- Then advance the pivot index, so the previous
    -- element is in the "smaller" part of the array.
    put (pivotIndex + 1)

-- Partition the array (between indices 'start' and 'end')
-- Return the final pivot index. All elements to the left
-- of that index will be smaller than the element there.
-- All elements to the right will be greater.
partition :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => a i e -> i -> i -> m i
partition arr start end = do
  -- Read the first element, this will be our "pivot element".
  pivotElement <- readArray arr start
  let pivotIndex_0 = start + 1
  -- Loop through all the elements and swap them into the proper side
  -- of the array.
  finalPivotIndex <- execStateT
    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])
    pivotIndex_0
  -- The element at 'finalPivotIndex' will be greater than our pivot element.
  -- (or else it is the end of the array)
  -- So swap our pivot element into the position before it.
  swap arr start (finalPivotIndex - 1)
  return $ finalPivotIndex - 1

-- Main driver of our algorithm.
-- Sorts a portion of the array, defined by 'start' and 'end'.
-- Base Case: If start + 1 >= end, we return immediately,
--            as the current segment is empty.
-- Note: 'end' refers to the element "one past the end" of our segment.
quicksortHelper :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => i -> i -> a i e -> m ()
quicksortHelper start end arr = when (start + 1 < end) $ do
  -- Partition the array
  pivotIndex <- partition arr start end
  -- Recursively sort each half.
  -- (The pivot element is already in its correct position)
  quicksortHelper start pivotIndex arr
  quicksortHelper (pivotIndex + 1) end arr

-- Runs the full quicksort.
-- This is just a wrapper, calling out to the helper function
-- but using the full bounds of the array.
quicksort :: (Ord e, Enum i, Num i, Ix i, MArray a e m) => a i e -> m ()
quicksort arr = do
  (minIndex, maxIndex) <- getBounds arr
  quicksortHelper minIndex (maxIndex + 1) arr

-- Quicksort, specialized to a pure structure, using STArray.
quicksortPure :: (Ord e, Enum i, Num i, Ix i) => Array i e -> Array i e
quicksortPure input = runSTArray $ do
  arr <- thaw input
  quicksort arr
  return arr

-- Specialized type signature for IOArray.
quicksortIO :: (Ord e, Enum i, Num i, Ix i) => IOArray i e -> IO ()
quicksortIO = quicksort

unsorted :: Array Int Int
unsorted = listArray (0, 10) [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

unsortedIO :: IO (IOArray Int Int)
unsortedIO = newListArray (0, 10) [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

run :: IO ()
run = do
  putStrLn "Pure"
  let result = quicksortPure unsorted
  print (elems result)
  putStrLn "IO"
  ioArr <- unsortedIO
  quicksortIO ioArr
  getElems ioArr >>= print
