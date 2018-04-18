import time
from concurrent.futures import Future, ThreadPoolExecutor

with ThreadPoolExecutor() as ex:

    def thunk():
        time.sleep(0.3)
        print("end")
        return "ok"

    print("start")
    fut = ex.submit(thunk)

print("use", fut.result())
print("use2", fut.result())
