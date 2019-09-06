import sys
import itertools
from collections import namedtuple

S = namedtuple("S", "L, R, t, history")


def trim(L, xs):
    return [y for y in L if y not in xs]


def forward(s):
    return [
        S(trim(s.L, pair), [*s.R, *pair], max(*pair) + s.t, [*s.history, ["F", *pair]])
        for pair in itertools.combinations(s.L, 2)
    ]


def backward(s):
    return [
        S([*s.L, *one], trim(s.R, one), one[0] + s.t, [*s.history, ["B", one]])
        for one in itertools.combinations(s.R, 1)
    ]


def calc(s):
    for s in forward(s):
        if len(s.L) == 0:
            yield s
            continue
        for s in backward(s):
            yield from calc(s)


L = [int(x) for x in sys.argv[1:]]
acc = calc(S(L, [], 0, []))
for s in sorted(acc, key=lambda s: s.t):
    print(s.t, s.history)
    break
