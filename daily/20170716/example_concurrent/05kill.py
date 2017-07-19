import concurrent.futures as f
import logging

logging.basicConfig(level=logging.DEBUG)


def fn(n):
    import os
    import signal
    pid = os.getpid()
    print("start", n, pid)
    os.kill(pid, signal.SIGKILL)

    # ここに来ることは無い
    0 / 1
    return None


print("----------------------------------------")
with f.ProcessPoolExecutor() as e:
    futs = []
    for i in range(10):
        futs.append(e.submit(fn, i))
    for fut in f.as_completed(futs):
        print(fut.result())
print("----------------------------------------")
