import pandas as pd

d = [
    {
        "query": "a",
        "impressions": 10000,
        "memo": "hmm",
    },
    {
        "query": "b",
        "impressions": 1000,
        "memo": "hai",
    },
]
df = pd.DataFrame(d, columns=["query", "impressions"])
print(df)
print(df)
