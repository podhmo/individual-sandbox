from merge import merge

xs = [{"category": 2.7, "x": 10}, {"category": 3.7, "x": 10}]
ys = [{"category": 3.7, "y": 10}, {"category": 3.8, "y": 10}]

print("inner")
print(merge(xs, ys, on="category"))
print("outer")
print(merge(xs, ys, how="outer", on="category"))
print("left")
print(merge(xs, ys, how="left", on="category"))
print("right")
print(merge(xs, ys, how="right", on="category"))

# -- stdout --------------------
# >> inner
# >> [{'category': 3.7, 'y': 10, 'x': 10}]
# >> outer
# >> [{'category': 3.7, 'y': 10, 'x': 10}, {'category': 3.8, 'y': 10, 'x': None}, {'category': 2.7, 'x': 10, 'y': None}]
# >> left
# >> [{'category': 2.7, 'x': 10, 'y': None}, {'category': 3.7, 'x': 10, 'y': 10}]
# >> right
# >> [{'category': 3.7, 'y': 10, 'x': 10}, {'category': 3.8, 'y': 10, 'x': None}]
