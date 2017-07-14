from concurrent.futures import ProcessPoolExecutor, as_completed


def get(i):
    import time
    time.sleep(21)
    return i * i

with ProcessPoolExecutor(10) as e:
    futs = []
    for i in range(20):
        print("add", i)
        futs.append(e.submit(get, i))
    for f in as_completed(futs):
        print("result", f.result())
