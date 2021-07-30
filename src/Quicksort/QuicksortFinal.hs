module QuicksortFinal where

-- Same "in-place" algorithm as QuicksortInPlace, but now uses
-- a randomized pivot index when partitioning, so that we don't
-- get slow O(n^2) behavior when input is already sorted.

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Ix
import System.Random

swap :: (Num i, Ix i, MArray a e m) => a i e -> i -> i -> m ()
swap arr i j = do
  elem1 <- readArray arr i
  elem2 <- readArray arr j
  writeArray arr i elem2
  writeArray arr j elem1

partitionLoop :: (Ord e, Num i, Ix i, MArray a e m)
  => a i e -> e -> i -> StateT i m ()
partitionLoop arr pivotElement i = do
  pivotIndex <- get
  currentElement <- lift $ readArray arr i
  when (currentElement <= pivotElement) $ do
    lift $ swap arr i pivotIndex
    put (pivotIndex + 1)

partition :: (Ord e, Enum i, Num i, Ix i, Random i, MArray a e m) => a i e -> i -> i -> StdGen -> m (i, StdGen)
partition arr start end gen1 = do
  -- Pick a random element, and swap it into the start of the array.
  let (indexForPivotElement, gen2) = randomR (start, end - 1) gen1
  swap arr start indexForPivotElement
  -- Now proceed with partitioning as before.
  pivotElement <- readArray arr start
  let pivotIndex_0 = start + 1
  finalPivotIndex <- execStateT
    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])
    pivotIndex_0
  swap arr start (finalPivotIndex - 1)
  -- Must return the new generator.
  return $ (finalPivotIndex - 1, gen2)

quicksortHelper :: (Ord e, Enum i, Num i, Ix i, Random i, MArray a e m) => i -> i -> a i e -> StdGen -> m StdGen
quicksortHelper start end arr gen1 = if start + 1 >= end then return gen1
 else do
  -- Each call here will produce a new random generator, so we must
  -- thread this through.
  (pivotIndex, gen2) <- partition arr start end gen1
  gen3 <- quicksortHelper start pivotIndex arr gen2
  quicksortHelper (pivotIndex + 1) end arr gen3

quicksort :: (Ord e, Enum i, Num i, Ix i, Random i, MArray a e m) => a i e -> StdGen -> m ()
quicksort arr gen = do
  (minIndex, maxIndex) <- getBounds arr
  void $ quicksortHelper minIndex (maxIndex + 1) arr gen

quicksortPure :: (Ord e, Enum i, Num i, Ix i, Random i) => Array i e -> StdGen -> Array i e
quicksortPure input gen = runSTArray $ do
  arr <- thaw input
  void $ quicksort arr gen
  return arr

quicksortIO :: (Ord e, Enum i, Num i, Ix i, Random i) => IOArray i e -> StdGen -> IO ()
quicksortIO = quicksort

unsorted :: Array Int Int
unsorted = listArray (0, 10) [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

unsortedIO :: IO (IOArray Int Int)
unsortedIO = newListArray (0, 10) [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

run :: IO ()
run = do
  putStrLn "Pure"
  gen1 <- getStdGen
  let result = quicksortPure unsorted gen1
  print (elems result)
  putStrLn "IO"
  gen2 <- getStdGen
  ioArr <- unsortedIO
  quicksortIO ioArr gen2
  getElems ioArr >>= print
