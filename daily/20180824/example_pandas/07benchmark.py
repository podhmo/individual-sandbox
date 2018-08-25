import timeit
setup = """
import pandas as pd
rows = [{"name": "foo", "age": 20}] * 1000
d = {"name": ["foo"]* 1000, "age": [20] * 1000}
"""

R = 1
N = 1
code = """
df = pd.DataFrame.from_records(rows)
"""
print(timeit.repeat(code, setup=setup, repeat=R, number=N))

code2 = """
df = pd.DataFrame.from_dict(d)
"""
print(timeit.repeat(code2, setup=setup, repeat=R, number=N))

code3 = """
df = pd.DataFrame.from_dict(d)

df2 = pd.DataFrame()
df2["name"] = df["name"].astype("category")
df2["age"] = df["age"].astype("uint8")

df.info(memory_usage="deep")
df2.info(memory_usage="deep")
"""
print(timeit.repeat(code3, setup=setup, repeat=R, number=N))
