import concurrent.futures as f
import logging
import multiprocessing as m
m.log_to_stderr(logging.DEBUG)
logging.basicConfig(level=logging.DEBUG)


def fn(n):
    import os
    import time
    pid = os.getpid()
    print("start", n, pid)
    time.sleep(1)
    # ここに来ることは無い
    return None


print("----------------------------------------")
try:
    with f.ProcessPoolExecutor() as e:
        futs = []
        r = []
        for i in range(10):
            futs.append(e.submit(fn, i))
        print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
        for fut in f.as_completed(futs):
            r.append(fut)
except Exception as e:
    print(type(e), repr(e), e)
print("########################################")
for fut in r:
    print(fut.result())
print("----------------------------------------")
