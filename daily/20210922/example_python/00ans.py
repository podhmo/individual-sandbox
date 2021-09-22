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


L = list(range(1, 11))
print(solve(L, 2))