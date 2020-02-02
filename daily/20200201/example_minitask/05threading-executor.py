import queue
import threading
import time
from minitask.q import Q
from minitask.q import PickleFormat
from minitask.q import consume
from handofcats import as_command


class Executor:
    def __init__(self):
        self.threads = []

    def spawn(self, target):
        th = threading.Thread(target=target)
        self.threads.append(th)
        th.start()

    def wait(self):
        for th in self.threads:
            th.join()


@as_command
def run():
    q = Q(queue.Queue(), format_protcol=PickleFormat())
    ex = Executor()

    def producer():
        for i in range(5):
            q.put(i)
            time.sleep(0.05)
        q.put(None)

    def worker():
        for i in consume(q):
            print(i)

    ex.spawn(producer)
    ex.spawn(worker)
    ex.wait()
    print("ok")
