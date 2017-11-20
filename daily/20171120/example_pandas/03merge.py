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

group_df = page_query_df.groupby("query")
df = group_df[["sessions"]].sum()
merged_df = df.merge(query_df, left_index=True, right_on="query", how="inner")
import pdb; pdb.set_trace()
print(merged_df)
