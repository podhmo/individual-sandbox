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
print(pd.DataFrame.from_dict(d))
