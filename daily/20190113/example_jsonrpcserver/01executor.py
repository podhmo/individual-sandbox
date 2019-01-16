import time
import logging
from concurrent.futures import ThreadPoolExecutor


def hello(name: str):
    t = time.time()
    print(name, "hello")
    time.sleep(1)
    print(name, "bye", time.time() - t)


logging.basicConfig(level=logging.DEBUG)
with ThreadPoolExecutor(max_workers=10) as ex:
    ex.submit(hello, "foo")
    ex.submit(hello, "bar")
print("ok")
