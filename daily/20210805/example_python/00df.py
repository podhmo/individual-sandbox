import pandas as pd

df = pd.read_json("./data.json", orient="records")
print(df)
lhs = df.pivot(index="id", columns="event_month", values=["v"])
rhs = df.drop(["event_month", "v"], axis=1).drop_duplicates()

print(lhs)
print(rhs)

print("-")
lhs.columns = lhs.columns.droplevel(0)
print(lhs.reset_index(level=0).merge(rhs, on="id"))
