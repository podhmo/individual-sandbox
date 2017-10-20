import os
import sys
import threading
import time
import signal
import traceback


def main():
    p = os.getpid()

    def ping():
        os.kill(p, signal.SIGUSR1)

    def print_stack(signum, tb):
        f = sys._getframe(1)
        print("----------------------------------------")
        traceback.print_stack(f, limit=None, file=sys.stdout)

    signal.signal(signal.SIGUSR1, print_stack)

    stop = False
    interval = 0.5

    def tick():
        nonlocal stop
        ping()
        if not stop:
            th = threading.Timer(interval, tick)
            th.setDaemon(True)
            th.start()
            return th

    tick()
    f(1)
    f(2)
    f(3)


def f(n):
    for i in range(n):
        g(i)
        time.sleep(0.5)


def g(n):
    for i in range(n):
        h(i)
        time.sleep(0.5)


def h(n):
    for i in range(n):
        h(i)
        time.sleep(0.5)


if __name__ == "__main__":
    main()
