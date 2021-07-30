import random

def swap(arr, i, j):
  temp = arr[i]
  arr[i] = arr[j]
  arr[j] = temp

def partition(arr, start, end):
  indexForPivotElement = random.randint(start, end - 1)
  swap(arr, start, indexForPivotElement)
  pivotElement = arr[start]
  pivotIndex = start + 1
  for i in range(start + 1, end):
    if arr[i] <= pivotElement:
      swap(arr, i, pivotIndex)
      pivotIndex += 1
  swap(arr, start, pivotIndex - 1)
  return pivotIndex - 1

def quicksortHelper(arr, start, end):
  if start + 1 >= end:
    return

  pivotIndex = partition(arr, start, end)
  quicksortHelper(arr, start, pivotIndex)
  quicksortHelper(arr, pivotIndex + 1, end)

def quicksort(arr):
  quicksortHelper(arr, 0, len(arr))

my_array = [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]
quicksort(my_array)
print(my_array)
