import concurrent.futures as f
import logging

logging.basicConfig(level=logging.DEBUG)

def fn(n):
    print("start", n)
    1 / 0


print("----------------------------------------")
with f.ProcessPoolExecutor() as e:
    futs = []
    for i in range(2):
        futs.append(e.submit(fn, i))

    for fut in f.as_completed(futs):
        try:
            print(fut.result())
        except ZeroDivisionError:
            logging.info("hmm", exc_info=True)
            print("hmm")
print("----------------------------------------")
