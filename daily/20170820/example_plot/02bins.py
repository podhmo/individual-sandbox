bins = list(range(20, 61, 5))
expected = [22.5, 27.5, 32.5, 37.5, 42.5, 47.5, 52.5, 57.5]
actual = [(x + y) / 2 for x, y in zip(bins, bins[1:])]

print(bins)
print(actual)
assert actual == expected
