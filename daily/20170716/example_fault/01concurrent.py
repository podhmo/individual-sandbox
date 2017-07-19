import concurrent.futures as f
import logging
import multiprocessing as m
m.log_to_stderr(logging.DEBUG)
logging.basicConfig(level=logging.DEBUG)


def fn(n):
    import ctypes
    libfault = ctypes.cdll.LoadLibrary("libfault.dylib")
    print("start", n)
    libfault.disp(n * n)
    # ここに来ることは無い
    return None


print("----------------------------------------")
with f.ProcessPoolExecutor() as executor:
    futs = []
    r = []
    for i in range(10):
        futs.append(executor.submit(fn, i))
    print(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
    for fut in f.as_completed(futs):
        r.append(fut)
print("########################################")
for fut in r:
    try:
        print(fut.result())
    except Exception as e:
        print(type(e), e)
executor.submit(fn, 0)
print("----------------------------------------")
