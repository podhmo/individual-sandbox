import timeit

setup = """
import numpy.random as rnd
import pandas as pd
m = rnd.randint(100, size=(500, 4))
"""

code = """pd.DataFrame(m)"""

print(code)
print(timeit.repeat(code, repeat=3, number=1000, setup=setup))

code2 = """pd.DataFrame.from_records(m)"""
print(code2)
print(timeit.repeat(code2, repeat=3, number=1000, setup=setup))

code3 = """pd.DataFrame.from_dict(m)"""
print(code3)
print(timeit.repeat(code3, repeat=3, number=1000, setup=setup))

"""
pd.DataFrame(m)
[0.05833214399171993, 0.05768375302432105, 0.05738733601174317]
pd.DataFrame.from_records(m)
[4.58411434001755, 4.612576251005521, 4.57562349899672]
pd.DataFrame.from_dict(m)
[0.0648788139806129, 0.05974143699859269, 0.059254093997878954]
"""
