import concurrent.futures as f
import logging

logging.basicConfig(level=logging.DEBUG)


def fn(n):
    import os
    import signal
    import time
    pid = os.getpid()
    print("start", n, pid)
    time.sleep(1)
    os.kill(pid, signal.SIGKILL)

    # ここに来ることは無い
    0 / 1
    return None


print("----------------------------------------")
with f.ProcessPoolExecutor() as e:
    futs = []
    r = []
    for i in range(100):
        futs.append(e.submit(fn, i))
    for fut in f.as_completed(futs):
        r.append(fut)
for fut in r:
    print(fut.result())
print("----------------------------------------")
