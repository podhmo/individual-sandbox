import sys
import time
import queue
import pickle
import pathlib

rows = [
    {"method": "foo", "args": [1], "kwargs": {}},
    {"method": "foo", "args": [2], "kwargs": {}},
    {"method": "foo", "args": [3], "kwargs": {}},
    {"method": "foo", "args": [4], "kwargs": {}},
]

q = queue.Queue()
path = pathlib.Path("03.pickle")
if path.exists():
    print("load", file=sys.stderr)
    items = pickle.load(path.open("rb"))
    for row in items:
        q.put(row)
else:
    print("init", file=sys.stderr)
    for row in rows:
        q.put(row)
    q.put(None)
try:
    while True:
        row = q.get()
        if row is None:
            q.task_done()
            break
        print(row)
        time.sleep(1)
        q.task_done()

    if path.exists():
        print("clear", file=sys.stderr)
        path.unlink()
except KeyboardInterrupt:
    print("save", file=sys.stderr)
    q.queue.appendleft(row)
    pickle.dump(q.queue, path.open("wb"))
