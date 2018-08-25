import pandas as pd

rows = {
    0: {
        "group": "x",
        "name": "foo",
        "age": 20
    },
    1: {
        "group": "x",
        "name": "bar",
        "age": 21
    },
    2: {
        "group": "y",
        "name": "boo",
        "age": 21
    },
}

df = pd.DataFrame.from_dict(rows, orient="index")
print(df)
print(df.index)
