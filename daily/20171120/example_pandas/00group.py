import pandas as pd

dfs = []
dfs.append(pd.DataFrame({"a": [1, 1, 1], "b": [2, 20, 300], "v": [3, 30, 300]}))
dfs.append(pd.DataFrame({"a": [2], "b": [2], "v": [3]}))

summary_df = pd.concat(dfs).groupby("a")
group_df = summary_df[["v"]].sum()
group_df["min_v"] = summary_df[["v"]].min()

print(pd.DataFrame({"x": [1, 2]})[["x"]])
group_df["xxxx"] = [1, 2]

print(group_df)
