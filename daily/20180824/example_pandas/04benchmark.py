import timeit

setup = """
import numpy.random as rnd
import pandas as pd
m = rnd.randint(100, size=(4, 500))
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
[0.0612281680223532, 0.061058969993609935, 0.06033549702260643]
pd.DataFrame.from_records(m)
[20.55791396400309, 20.584746097010793, 20.574114716000622]
pd.DataFrame.from_dict(m)
[0.07000833298661746, 0.06267698100418784, 0.06902250897837803]
"""
