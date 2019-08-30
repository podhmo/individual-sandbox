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


def calc(s, *, mem):
    k = tuple(sorted(s.L))
    t0 = s.t
    if k in mem:
        ans = mem[k]
        yield S(
            L=[],
            R=s.L + s.R,
            history=s.history + ans.history[len(s.history) :],
            t=ans.t + s.t,
        )
        return
    for s in forward(s):
        if len(s.L) == 0:
            ans = S(L=k, R=[], history=s.history, t=s.t - t0)
            if k not in mem or ans.t < mem[k].t:
                mem[k] = ans
            yield s
            continue
        for s in backward(s):
            yield from calc(s, mem=mem)


L = [int(x) for x in sys.argv[1:]]
acc = calc(S(L, [], 0, []), mem={})
for s in acc:
    print(s.t, s.history)
