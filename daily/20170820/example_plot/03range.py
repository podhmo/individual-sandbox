import math


def adjusted(xs):
    minv = min(xs)
    lsize = math.floor(math.log10(minv))
    left = (minv // (10 ** lsize)) * (10 ** lsize)

    maxv = max(xs)
    rsize = math.floor(math.log10(maxv))
    right = ((maxv // (10 ** rsize)) + 1) * (10 ** rsize)

    return left, right


xs = [21, 30, 55]
ys = [11, 101, 88]

print(adjusted(xs))
print(adjusted(ys))
