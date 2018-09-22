import time
from concurrent.futures import (
    ThreadPoolExecutor,
    as_completed,
)


def req(i):
    print(f"{i}: start")
    time.sleep(0.5)
    print(f"{i}: end")
    return i


with ThreadPoolExecutor(max_workers=5) as ex:
    futs = [ex.submit(req, i) for i in range(13)]
    for fut in as_completed(futs):
        print(fut.result())
