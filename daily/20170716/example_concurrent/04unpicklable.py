import concurrent.futures as f
import concurrent.futures.process
import logging

logging.basicConfig(level=logging.DEBUG)


def fn(n):
    print("start", n)

    def display():
        print(n * n)

    return display


print("----------------------------------------")
with f.ProcessPoolExecutor() as e:
    futs = []
    for i in range(2):
        futs.append(e.submit(fn, i))

    for fut in f.as_completed(futs):
        try:
            print(fut.result())
        except concurrent.futures.process.BrokenProcessPool:
            logging.info("hmm", exc_info=True)
print("----------------------------------------")
