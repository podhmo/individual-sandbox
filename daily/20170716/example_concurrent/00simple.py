import time
import concurrent.futures as f


def fn(n):
    print("start", n)
    time.sleep(1)
    return n * n


print("----------------------------------------")
st = time.time()
with f.ProcessPoolExecutor() as e:
    futs = []
    for i in range(7):
        futs.append(e.submit(fn, i))

    for fut in f.as_completed(futs):
        print(fut.result())
print("----------------------------------------")
print("end", time.time() - st)
