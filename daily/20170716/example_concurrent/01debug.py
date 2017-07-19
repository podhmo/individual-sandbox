import logging
import time
import concurrent.futures as f
import multiprocessing as m

m.log_to_stderr(logging.DEBUG)


def fn(n):
    print("start", n)
    time.sleep(1)
    return n * n


logging.basicConfig(level=logging.DEBUG)
print("----------------------------------------")
with f.ProcessPoolExecutor() as e:
    futs = []
    for i in range(7):
        futs.append(e.submit(fn, i))

    for fut in f.as_completed(futs):
        print(fut.result())
print("----------------------------------------")

