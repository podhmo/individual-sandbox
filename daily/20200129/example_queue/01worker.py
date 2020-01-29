import threading
from functools import partial
import queue


def peek(q):
    while True:
        item = q.get()
        if item is None:
            q.task_done()
            break
        print(item)
        q.task_done()


q = queue.Queue()
for i in range(5):
    q.put(i)
q.put(None)

th = threading.Thread(target=partial(peek, q))
th.start()
q.join()
th.join()
print("ok")
