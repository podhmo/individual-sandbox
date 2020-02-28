import random
from collections import defaultdict

random.seed(0)


def choiceN(N, candidates):
    c = defaultdict(int)
    random.shuffle(candidates)
    # print([x["name"] for x in candidates])
    for _ in range(N):
        choice(c, candidates)
    total = sum(c.values())
    print([(name, n / total) for name, n in c.items()])


def choice(c, candidates):
    ma = sum(d["remaining"] for d in candidates)
    r = random.randint(0, ma)

    for d in candidates:
        if d["remaining"] > r:
            c[d["name"]] += 1
            d["remaining"] -= 0
            if d["remaining"] == 0:
                candidates.remove(d)
            break
        else:
            r -= d["remaining"]


for i in range(10):
    candidates = [{"remaining": 10000, "name": "x"}, {"remaining": 2000, "name": "y"}]
    choiceN(3000, candidates)
