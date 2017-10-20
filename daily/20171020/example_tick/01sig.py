import os
import sys
import threading
import time
import signal
import traceback


def main():
    def print_stack(signum, tb):
        f = sys._getframe(1)
        print("----------------------------------------")
        traceback.print_stack(f, limit=None, file=sys.stdout)

    signal.signal(signal.SIGINT, print_stack)

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
