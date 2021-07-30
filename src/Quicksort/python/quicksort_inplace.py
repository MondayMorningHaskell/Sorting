def swap(arr, i, j):
  temp = arr[i]
  arr[i] = arr[j]
  arr[j] = temp

# Partition the array (between indices 'start' and 'end')
# Return the final pivot index. All elements to the left
# of that index will be smaller than the element there.
# All elements to the right will be greater.
def partition(arr, start, end):
  # Read the first element, this will be our "pivot element".
  pivotElement = arr[start]
  pivotIndex = start + 1
  # Loop through all the elements and swap them into the proper side
  # of the array.
  for i in range(start + 1, end):
    # If the element at 'i' is smaller than the pivot element,
    # swap it into the smaller/left portion of the array, and
    # advance the pivot index.
    if arr[i] <= pivotElement:
      swap(arr, i, pivotIndex)
      pivotIndex += 1
  # The element at 'finalPivotIndex' will be greater than our pivot element.
  # (or else it is the end of the array)
  # So swap our pivot element into the position before it.
  swap(arr, start, pivotIndex - 1)
  return pivotIndex - 1

# Main driver of our algorithm.
# Sorts a portion of the array, defined by 'start' and 'end'.
# Base Case: If start + 1 >= end, we return immediately,
#            as the current segment is empty.
# Note: 'end' refers to the element "one past the end" of our segment.
def quicksortHelper(arr, start, end):
  if start + 1 >= end:
    return
  # Partition the array
  pivotIndex = partition(arr, start, end)
  # Recursively sort each half.
  # (The pivot element is already in its correct position)
  quicksortHelper(arr, start, pivotIndex)
  quicksortHelper(arr, pivotIndex + 1, end)

# Runs the full quicksort.
# This is just a wrapper, calling out to the helper function
# but using the full bounds of the array.
def quicksort(arr):
  quicksortHelper(arr, 0, len(arr))

my_array = [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]
quicksort(my_array)
print(my_array)
