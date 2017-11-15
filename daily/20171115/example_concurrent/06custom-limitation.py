import random
import time
from concurrent.futures import ThreadPoolExecutor, as_completed
from collections import defaultdict
from queue import Queue


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return i


def create_task_with_limitation(q, x):
    def do(fn, *args, **kwargs):
        q.put(x)
        r = fn(*args, **kwargs)
        return r

    return do, lambda x: q.get_nowait()


qmap = defaultdict(lambda: Queue(maxsize=2))
xs = ["a", "a", "a", "b", "b", "b", "b", "c"]

print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for x in xs:
        task, on_finished = create_task_with_limitation(qmap[x], x)
        fut = ex.submit(task, do_task, x)
        fut.add_done_callback(on_finished)
        futs.append(fut)
    for f in as_completed(futs):
        print("ok", f.result())
print("E")
