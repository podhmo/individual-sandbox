import queue
import threading
import time
from minitask.q import Q
from minitask.q import PickleFormat
from minitask.q import consume
from handofcats import as_command


@as_command
def run():
    q = Q(queue.Queue(), format_protcol=PickleFormat())
    ev = threading.Event()

    def producer():
        for i in range(5):
            q.put(i)
            time.sleep(0.05)
        q.put(None)
        ev.set()

    def worker():
        for i in consume(q):
            print(i)

    threading.Thread(target=producer).start()
    threading.Thread(target=worker).start()
    ev.wait()
    q.join()
    print("ok")
