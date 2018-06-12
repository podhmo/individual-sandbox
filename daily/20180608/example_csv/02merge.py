import pandas


def extract_main_metrics(df, endtoken):
    ks = []
    for k in df.columns:
        ks.append(k)
        if k == endtoken:
            break
    return df[ks]


df0 = pandas.read_csv("data/members.csv")
print(df0)
df1 = pandas.read_csv("data/groups.csv")
print(df1)

merged = df0.merge(df1, how="left", on="groupId")
print(merged)

merged2 = extract_main_metrics(df0, "name").merge(
    extract_main_metrics(df1, "name"), how="left", on="groupId"
)
print(merged2)
