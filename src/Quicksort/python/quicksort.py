def quicksort(arr):
  if len(arr) == 0:
    return []
  x = arr[0]
  smaller_sorted = quicksort(filter(lambda i: i <= x, arr[1:]))
  bigger_sorted = quicksort(filter(lambda i: i > x, arr[1:]))
  return smaller_sorted + [x] + bigger_sorted

unsorted = [1, 6, 8, 3, 4, 7, 11, 10, 2, 5, 9]
sorted = quicksort(unsorted)
print(sorted)
