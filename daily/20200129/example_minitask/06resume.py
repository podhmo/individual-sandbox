import time
import queue
import signal
import os


def callback(sig, tb):
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")
    print(sig, tb)
    print("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")


sig = signal.SIGHUP
signal.signal(sig, callback)

L = [1, 2, 3]
q = queue.Queue()
for x in L:
    q.put_nowait(x)
q.put_nowait(None)
try:
    while True:
        item = q.get()
        if item is None:
            break
        print(item)
        time.sleep(1)
except KeyboardInterrupt:
    os.kill(os.getpid(), signal.SIGHUP)
    breakpoint()
    print("hoi")
