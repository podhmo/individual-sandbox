from itertools import chain
xs = [["x", "y", "z"], ["a"]]

# n = 0
print(xs)
# n = 1
for x in xs:
    print(x)
# n = 2
for x in xs:
    for y in x:
        print(y)
