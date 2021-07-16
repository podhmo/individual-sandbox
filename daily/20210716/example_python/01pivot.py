import pandas as pd

data = [
    {"A": 1, "B": 1, "x": 1, "y": 1},
    {"A": 1, "B": 2, "x": 10, "y": 10},
    {"A": 1, "B": 3, "x": 100, "y": 100},
    {"A": 2, "B": 1, "x": 1, "y": 1},
    {"A": 2, "B": 2, "x": 10, "y": 10},
    {"A": 2, "B": 3, "x": 100, "y": 100},
    {"A": 3, "B": 1, "x": 1, "y": 1},
    {"A": 3, "B": 2, "x": 10, "y": 10},
    {"A": 3, "B": 3, "x": 100, "y": 100},
]

df = pd.DataFrame.from_records(data)
print(df)
#    A  B    x    y
# 0  1  1    1    1
# 1  1  2   10   10
# 2  1  3  100  100
# 3  2  1    1    1
# 4  2  2   10   10
# 5  2  3  100  100
# 6  3  1    1    1
# 7  3  2   10   10
# 8  3  3  100  100

data2 = [
    {"A": 1, "B": 1, "w": 20},
    {"A": 1, "B": 2, "w": 20},
    {"A": 1, "B": 3, "w": 20},
    {"A": 2, "B": 1, "w": 200},
    {"A": 2, "B": 2, "w": 200},
    {"A": 2, "B": 3, "w": 200},
    {"A": 3, "B": 1, "w": 2000},
    {"A": 3, "B": 2, "w": 2000},
    {"A": 3, "B": 3, "w": 2000},
]

df2 = pd.DataFrame.from_records(data2)
print(df2)
#    A  B   w
# 0  1  1  20
# 1  1  2  20
# 2  1  3  20
# 3  2  1  200
# 4  2  2  200
# 5  2  3  200
# 6  3  1  2000
# 7  3  2  2000
# 8  3  3  2000

df3 = df.merge(df2, on=("A", "B"))
print(df3)
#    A  B    x    y   w
# 0  1  1    1    1  20
# 1  1  2   10   10  20
# 2  1  3  100  100  20
# 3  2  1    1    1  200
# 4  2  2   10   10  200
# 5  2  3  100  100  200
# 6  3  1    1    1  2000
# 7  3  2   10   10  2000
# 8  3  3  100  100  2000


df3["x/w"] = df3[["x", "w"]].apply(lambda row: row[0] / row[1], axis=1)
df3["y/w"] = df3[["y", "w"]].apply(lambda row: row[0] / row[1], axis=1)
df3.drop(columns="w", inplace=True)
df3 = df3[["A", "B", "x", "x/w", "y", "y/w"]]
print(df3)
# df3["x/w"] = df3[["x", "y"]].apply(lambda x, w: x / w)
# df3["y/w"] = df.apply(lambda row: row["y"] / row["w"])
# print(df3)
