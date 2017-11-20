import time
from concurrent.futures import ThreadPoolExecutor, wait


def do_task(i):
    print("before", i)
    time.sleep(0.1 * i)
    print("after", i)
    return f"ok: {i}"


with ThreadPoolExecutor() as ex:
    futs = []
    for i in range(5):
        futs.append(ex.submit(do_task, i))

    waited = wait(futs, timeout=0.2)
    print("hmm", waited.not_done)
    for fut in waited.not_done:
        fut.cancel()
    for fut in waited.done:
        print(fut.result())

# before 0
# after 0
# before 1
# before 2
# before 3
# before 4
# after 1
# after 2
# hmm {<Future at 0x10bfe89b0 state=running>, <Future at 0x10c0f2748 state=running>}
# ok: 2
# ok: 0
# ok: 1
# after 3
# after 4
