from dictknife.query import join, how_full_outer_join
from dictknife.jsonknife import json_pointer_to_path
from dictknife import Accessor

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


def mget(d, path, *, a=Accessor()):
    return a.maybe_access(d, json_pointer_to_path(path))


a = Accessor()
rows = list(join(x_packages, y_packages, on="version", how=how_full_outer_join))
for row in rows:
    print(
        {
            "version": mget(row, "0/version") or mget(row, "1/version"),
            "x_downloads": mget(row, "0/downloads"),
            "y_downloads": mget(row, "1/downloads"),
        }
    )
