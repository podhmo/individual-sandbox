import queue
import threading


def as_iterator(q):
    while True:
        v = q.get()
        yield v
        q.task_done()


def consume(itr):
    for v in itr:
        print("*", v)


q = queue.Queue()
th = threading.Thread(target=lambda: consume(as_iterator(q)), daemon=True)
th.start()
q.put(1)
q.put(2)
q.put(3)
q.put(4)
q.join()
print("ok")
