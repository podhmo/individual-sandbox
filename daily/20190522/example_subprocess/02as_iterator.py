import time
import queue
import threading
import subprocess


def as_iterator(q):
    while True:
        yield q.get()
        q.task_done()


def as_queue(p, *, timeout=None):
    running = True
    q = queue.Queue()

    def _stop():
        nonlocal running
        running = False

    def _reader():
        nonlocal running
        while running:
            q.put(p.stdout.readline())

    th = threading.Thread(target=_reader, daemon=True)
    th.start()
    return q, _stop


cmd = [
    "python",
    "-u",
    "-c",
    "import time; [print(i) or time.sleep(0.4) for i in range(10)]",
]
with subprocess.Popen(cmd, text=True, stdout=subprocess.PIPE) as p:
    q, stop = as_queue(p)
    for line in as_iterator(q):
        if not line:
            stop()
            break
        print("* ", time.time(), line.rstrip())
