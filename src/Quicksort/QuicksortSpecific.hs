module QuicksortSpecific where

-- Same code as QuicksortInPlace, but everything
-- is specialized to IOArray Int Int and the IO monad, so
-- that there are no constraints in the type signatures.

import Control.Monad
import Control.Monad.State
import Data.Array
import Data.Array.IO
import Data.Array.MArray
import Data.Array.ST
import Data.Ix

swap :: IOArray Int Int -> Int -> Int -> IO ()
swap arr i j = do
  elem1 <- readArray arr i
  elem2 <- readArray arr j
  writeArray arr i elem2
  writeArray arr j elem1

partitionLoop :: IOArray Int Int -> Int -> Int -> StateT Int IO ()
partitionLoop arr pivotElement i = do
  pivotIndex <- get
  currentElement <- lift $ readArray arr i
  when (currentElement <= pivotElement) $ do
    lift $ swap arr i pivotIndex
    put (pivotIndex + 1)

partition :: IOArray Int Int -> Int -> Int -> IO Int
partition arr start end = do
  pivotElement <- readArray arr start
  let pivotIndex_0 = start + 1
  finalPivotIndex <- execStateT
    (mapM (partitionLoop arr pivotElement) [(start + 1)..(end - 1)])
    pivotIndex_0
  swap arr start (finalPivotIndex - 1)
  return $ finalPivotIndex - 1

quicksortHelper :: Int -> Int -> IOArray Int Int -> IO ()
quicksortHelper start end arr = when (start + 1 < end) $ do
  pivotIndex <- partition arr start end
  quicksortHelper start pivotIndex arr
  quicksortHelper (pivotIndex + 1) end arr

quicksort :: IOArray Int Int -> IO ()
quicksort arr = do
  (minIndex, maxIndex) <- getBounds arr
  quicksortHelper minIndex (maxIndex + 1) arr

unsortedIO :: IO (IOArray Int Int)
unsortedIO = newListArray (0, 10) [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]

run :: IO ()
run = do
  putStrLn "IO"
  ioArr <- unsortedIO
  quicksort ioArr
  getElems ioArr >>= print
