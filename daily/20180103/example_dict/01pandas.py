import pandas as pd

user_df = pd.read_csv("data/users.csv")
group_df = pd.read_csv("data/groups.csv")

df = user_df.merge(group_df, left_on="group_id", right_on="id", suffixes=("", "_g"))
print(df)
