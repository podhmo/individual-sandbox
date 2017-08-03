# from multiprocessing import Queue
# from multiprocessing.queues import Empty
import sys
from queue import Queue, Empty
from threading import Thread
import subprocess
ON_POSIX = 'posix' in sys.builtin_module_names


def enqueue_output(out, queue):
    for line in iter(out.readline, b''):
        queue.put(line)
    out.close()


def getOutput(outQueue):
    buf = []
    try:
        while True:  # Adds output from the Queue until it is empty
            buf.append(outQueue.get_nowait())
    except Empty:
        return "".join(buf)


p = subprocess.Popen(
    ["python"],
    stdin=subprocess.PIPE,
    stdout=subprocess.PIPE,
    stderr=subprocess.PIPE,
    bufsize=1,
    shell=False,
    close_fds=ON_POSIX,
    universal_newlines=True
)

outQueue = Queue()
errQueue = Queue()

outThread = Thread(target=enqueue_output, args=(p.stdout, outQueue))
errThread = Thread(target=enqueue_output, args=(p.stderr, errQueue))

outThread.daemon = True
errThread.daemon = True

outThread.start()
errThread.start()

someInput = input("Input: ")

p.stdin.write(someInput)
p.stdin.write("\n")
p.stdin.flush()
for i in range(5):
    errors = getOutput(errQueue)
    output = getOutput(outQueue)
    print("{!r} {!r} {} {}".format(errors, output, type(errors), type(output)))
    import time
    time.sleep(0.1)
