import time
import concurrent.futures as f


def fn(n):
    print("start", n)
    time.sleep(1)
    return n * n


print("----------------------------------------")
st = time.time()
with f.ProcessPoolExecutor() as e:
    answers = e.map(fn, range(7))
    for i in answers:
        print(i)
print("----------------------------------------")
print("end", time.time() - st)
