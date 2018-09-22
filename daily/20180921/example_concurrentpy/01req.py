import time
from functools import partial
from concurrent.futures import (
    ThreadPoolExecutor,
    as_completed,
)


def req(i):
    print(f"{i}: start")
    time.sleep(0.5)
    print(f"{i}: end")
    return i


def heavy_req(i):
    print(f"{i}: Hstart")
    time.sleep(2)
    print(f"{i}: Hend")
    return i


with ThreadPoolExecutor(max_workers=5) as ex:
    fns = [
        partial(req, 0),
        partial(heavy_req, 1),
        partial(req, 2),
        partial(req, 3),
        partial(req, 4),
        partial(req, 5),
    ]
    futs = {ex.submit(fn): i for i, fn in enumerate(fns)}
    r = [None] * len(futs)

    for fut in as_completed(futs.keys()):
        r[futs[fut]] = fut.result()
    print(r)
