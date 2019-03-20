import signal
import time


def handler(signum, tb):
    print("@", signum, tb)


signal.signal(signal.SIGINT, handler)
for i in range(10):
    print("hello", i)
    time.sleep(1)
