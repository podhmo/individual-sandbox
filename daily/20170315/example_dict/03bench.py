import timeit


setup = """
import random
# random.seed(1)
xs = [frozenset([random.randint(0, 20) for _ in range(random.randint(1, 11))]) for _ in range(100)]

def classify(x, ys):
    subset, superset = [], []
    for y in ys:
        if y.issubset(x):
            subset.append(y)
        if y.issuperset(x):
            superset.append(y)
    return {"U": superset, "L": subset}
"""


for n in timeit.repeat("[classify(x, xs) for x in xs]", setup=setup, number=10, repeat=10):
    print(n)

"""
0.032358566008042544
0.0326823010109365
0.03170792500895914
0.03554409100615885
0.03236002300400287
0.03111618000548333
0.03140575399447698
0.03175140600069426
0.032073939000838436
0.030890166002791375
"""
