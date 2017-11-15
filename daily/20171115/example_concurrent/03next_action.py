import random
import time
from concurrent.futures import ThreadPoolExecutor, as_completed


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return "ok {i}".format(i=i)


def next_action(msg):
    print("catch:", msg)
    return msg


print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(5):
        futs.append(ex.submit(do_task, i))

    for fut in as_completed(futs):
        next_action(fut.result())
print("E")
