from dictknife.query import join, how_full_outer_join, Options
from dictknife import loading


x_packages = [
    {"version": "2.7", "downloads": 1000},
    {"version": "3.5", "downloads": 2000},
    {"version": "3.6", "downloads": 3000},
    {"version": "3.7", "downloads": 3000},
]
y_packages = [
    {"version": "3.5", "downloads": 2000},
    {"version": "3.6", "downloads": 2000},
    {"version": "3.7", "downloads": 2000},
    {"version": "3.8", "downloads": 500},
]
z_packages = [
    {"version": "3.7", "downloads": 20000},
    {"version": "3.8", "downloads": 500000},
    {"version": "3.9", "downloads": 500000},
]


def mget(d, k):
    if d is None:
        return None
    elif isinstance(d, (tuple, list)):
        for x in d:
            if x is None:
                continue
            return mget(x, k)
    return d.get(k)


rows = list(join(x_packages, y_packages, on="version", how=how_full_outer_join))
rows2 = list(
    join(
        rows,
        z_packages,
        right_on="version",
        left_on=lambda p: mget(p, "version"),
        how=how_full_outer_join,
    )
)

results = []
for x_and_y, z in rows2:
    if x_and_y is None:
        x = y = None
    else:
        x, y = x_and_y
    results.append(
        {
            "version": mget([x, y, z], "version"),
            "x_downloads": x["downloads"] if x else "",
            "y_downloads": y["downloads"] if y else "",
            "z_downloads": z["downloads"] if z else "",
        }
    )


loading.dumpfile(sorted(results, key=lambda x: x["version"]), format="md")
