import random
import time
from concurrent.futures import ThreadPoolExecutor, as_completed


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    1 / 0


print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(2):
        futs.append(ex.submit(do_task, i))
    for f in as_completed(futs):
        print(f.exception())
print("hmm")
with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(2):
        futs.append(ex.submit(do_task, i))
    for f in as_completed(futs):
        print(f.result())
print("E")
