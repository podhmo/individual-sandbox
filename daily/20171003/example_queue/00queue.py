import time
from queue import Queue


def fetch(q):
    print("before", time.time())
    r = q.get()
    print("after", time.time())
    print("----")
    return r


q = Queue()
for i in range(3):
    q.put(i)
print("start", time.time())
print("----")
fetch(q)
fetch(q)
fetch(q)
# fetch(q)  # getはblock. get_nowait()のときにはqueue.Emptyの例外

