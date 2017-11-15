import random
import time
from concurrent.futures import ThreadPoolExecutor


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))


print("S")
with ThreadPoolExecutor() as ex:
    for i in range(5):
        ex.submit(do_task, i)
print("E")
