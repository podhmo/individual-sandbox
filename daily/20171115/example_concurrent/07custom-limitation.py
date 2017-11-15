import random
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from threading import Semaphore
from collections import defaultdict


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return i


def with_limitation(sem):
    def do(fn, *args, **kwargs):
        with sem:
            return fn(*args, **kwargs)

    return do


sem_map = defaultdict(lambda: Semaphore(2))
xs = ["a", "a", "a", "b", "b", "b", "b", "c"]

print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for x in xs:
        futs.append(ex.submit(with_limitation(sem_map[x]), do_task, x))
    for f in as_completed(futs):
        print("ok", f.result())
print("E")
