import random
import time
from concurrent.futures import ThreadPoolExecutor, TimeoutError


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))


print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(5):
        futs.append(ex.submit(do_task, i))
    for f in futs:
        try:
            f.result(timeout=0)
        except TimeoutError as e:
            print("yay", type(e))
print("E")
