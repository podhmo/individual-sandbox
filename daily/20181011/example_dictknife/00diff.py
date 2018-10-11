from dictknife.diff import diff_rows

d0 = {
    "x": 10,
    "y": 100,
    "nested": {
        "v1": 0,
        "v2": 1,
        "vs": [0, 1, 2, 3],
    },
}
d1 = {
    "x": 10,
    "y": 110,
    "nested": {
        "v1": 10,
        "v2": -1,
        "vs": [0, 2, 2, 2],
    },
}

for row in diff_rows(d0, d1):
    if row["diff"] in ("", 0):
        continue
    print(row)
