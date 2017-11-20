import pandas as pd

query_df = pd.DataFrame({
    "query": ["a", "b"],
    "impressions": [10000, 200],
})

page_query_df = pd.DataFrame(
    {
        "query": ["a", "a", "c"],
        "path": ["/", "/index", "/"],
        "sessions": [1000, 8000, 100],
    }
)

group_df = page_query_df.groupby("query", as_index=False)
df = group_df[["sessions"]].sum()
merged_df = df.merge(query_df, left_on="query", right_on="query", how="inner")
print(merged_df)
