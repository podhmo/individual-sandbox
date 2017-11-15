import random
import time
from concurrent.futures import ThreadPoolExecutor, wait, FIRST_COMPLETED


def do_task(i):
    print("before {i}".format(i=i))
    time.sleep(1.05 * random.random())
    print("after {i}".format(i=i))
    return "response: {i}".format(i=i)


print("S")
with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(5):
        futs.append(ex.submit(do_task, i))
    r = wait(futs, return_when=FIRST_COMPLETED)
    for f in r.not_done:
        f.cancel()
    for f in r.done:
        print("ok", f.result())
print("E")
