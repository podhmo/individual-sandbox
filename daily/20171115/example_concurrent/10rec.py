import random
import time
from concurrent.futures import ThreadPoolExecutor, wait
from collections import defaultdict
from threading import Semaphore
from queue import Queue


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return list(range(i))


def with_limitation(sem):
    def do(fn, *args, **kwargs):
        with sem:
            return fn(*args, **kwargs)

    return do


def add_new_tasks(finished):
    for f in finished:
        xs = f.result()
        print("ok", len(xs))
        for x in xs:
            q.put(x)
    return []


q = Queue()
xs = [2, 2, 2, 2, 3, 4, 3, 4, 3, 4, 1, 5, 2]
sem_map = defaultdict(lambda: Semaphore(3))

print("S")
# enqueue
for x in xs:
    q.put(x)

with ThreadPoolExecutor() as ex:
    futs = []
    while True:
        while not q.empty():
            x = q.get()
            futs.append(ex.submit(with_limitation(sem_map[x]), do_task, x))

        r = wait(futs, timeout=0.3)
        futs.clear()
        futs.extend(r.not_done)
        if not futs and q.empty():
            break
        if r.done:
            futs.append(ex.submit(add_new_tasks, r.done))
        print("*hmm*")
print("E")
