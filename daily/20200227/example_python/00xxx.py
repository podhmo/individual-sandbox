import random
from collections import defaultdict

random.seed(0)
c = defaultdict(int)
candidates = [{"remaining": 1000, "name": "x"}, {"remaining": 200, "name": "y"}]
candidates = [{"remaining": 200, "name": "y"}, {"remaining": 1000, "name": "x"}]

# 4,6,10,20
# 0.2, (20-x) < 6 = 0.6
# 6,4,10
# 0.6, (10-x) < 4


def get():
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


for i in range(500):
    get()
print(c)
print(candidates)
