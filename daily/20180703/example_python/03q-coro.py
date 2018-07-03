import queue
import threading
import queue


def as_coro(q):
    yield None
    while True:
        print("ho")
        v = q.get_nowait()
        v = yield v
        q.task_done()
        q.put(v)


q = queue.Queue()
c = as_coro(q)
next(c)
print(c.send(1))
