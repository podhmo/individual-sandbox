from dictknife.query import join, how_full_outer_join
from dictknife import Accessor
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
]


mget = Accessor().maybe_access
rows = list(join(x_packages, y_packages, on="version", how=how_full_outer_join))
rows2 = list(
    join(
        rows,
        z_packages,
        right_on="version",
        left_on=lambda p: mget(p, [0, "version"]) or mget(p, [1, "version"]),
        how=how_full_outer_join,
    )
)
results = [
    {
        "version": mget(x, ["version"]) or mget(y, ["version"]) or mget(z, ["version"]),
        "x_downloads": mget(x, ["downloads"]) or "",
        "y_downloads": mget(y, ["downloads"]) or "",
        "z_downloads": mget(z, ["downloads"]) or "",
    }
    for ((x, y), z) in rows2
]


loading.dumpfile(sorted(results, key=lambda x: x["version"]), format="md")
