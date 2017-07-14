import logging
from concurrent.futures import ProcessPoolExecutor, as_completed


def get(i):
    import os
    print(os.getpid())
    import time
    time.sleep(10)
    return i * i


try:
    with ProcessPoolExecutor(10) as e:
        futs = []
        for i in range(20):
            print("add", i)
            f = e.submit(get, i)
            futs.append(f)

        for f in as_completed(futs):
            try:
                print("result", f.result())
            except Exception as e:
                logging.debug(str(e), exc_info=True)
except Exception as e:
    print("@", e)

logging.basicConfig(level=logging.DEBUG)
