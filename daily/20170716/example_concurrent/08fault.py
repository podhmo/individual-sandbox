import concurrent.futures as f
import logging
import multiprocessing as m

m.log_to_stderr(logging.DEBUG)
logging.basicConfig(level=logging.DEBUG)


def fn(n):
    def _fib(n):
        if n <= 1:
            return 1
        else:
            return _fib(n - 1) + _fib(n - 2)

    import sys
    sys.setrecursionlimit(10)
    return _fib(n)


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
