from dataclasses import dataclass


@dataclass
class Cell:
    xs: list
    sum: int


def solve(L, n):
    candidates = [Cell(xs=[], sum=0) for i in range(n)]
    for x in sorted(L, reverse=True):
        p = min(candidates, key=lambda p: p.sum + x)
        p.xs.append(x)
        p.sum = p.sum + x
    return candidates


def solve2(L, n):
    def gen(ys):
        if len(ys) == 0:
            return [[() for _ in range(n)]]
        else:
            return [
                [(ys[0], *xs) if i == j else xs for j, xs in enumerate(xss)]
                for xss in gen(ys[1:])
                for i in range(n)
            ]

    ans = min(gen(L), key=lambda xss: sum([(n := sum(xs)) * n for xs in xss]))
    return [Cell(xs=sorted(xs, reverse=True), sum=sum(xs)) for xs in ans]


import random

for _ in range(10):
    L = list([random.randint(1, 10) for _ in range(5)])
    # L = sorted([59, 20, 31, 34, 44, 100, 13, 5, 38, 32])
    # L = [1, 5, 6, 7, 8, 10]
    L = [5,7,7,9,10]
    print(sorted(L))
    ans = sorted(solve(L, 2), key=lambda c: c.sum)
    ans2 = sorted(solve2(L, 2), key=lambda c: c.sum)

    if sorted(ans, key=lambda c: c.sum) != sorted(ans2, key=lambda c: c.sum):
        print("  ", ans)
        print("  ", ans2)
