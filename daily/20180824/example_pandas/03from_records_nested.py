import pandas as pd
rows = iter([
    {
        "name": "foo",
        "statusChangedAt": {
            "start": 0,
            "finished": 10
        }
    },
    {
        "name": "boo",
        "statusChangedAt": {
            "start": 0,
            "finished": 100
        }
    },
])

df = pd.DataFrame.from_records(rows)
print(df)
