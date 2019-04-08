# broken

import queue
import threading
from concurrent import futures


class Executor:
    def __init__(self, n):
        self.pool = [
            threading.Thread(target=self.do_task, daemon=True) for i in range(n)
        ]
        self.q = queue.Queue()

    def start(self):
        for th in self.pool:
            th.start()

    def do_task(self):
        fn = self.q.get()
        fn()


def fut(ex, fn):
    fut = futures.Future()

    def task():
        try:
            r = fn()
            fut.set_result(r)
        except Exception as e:
            fut.set_exception(e)

    ex.q.put(task)
    return fut


def main():
    x = yield fut()
    y = yield fut()
    return x + y


loop.run(main())
