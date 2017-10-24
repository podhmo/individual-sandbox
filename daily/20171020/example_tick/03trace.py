import sys
import traceback


def mytrace():
    print("----------------------------------------")
    f = sys._getframe(1)
    traceback.print_stack(f)


def f():
    g()


def g():
    mytrace()


f()
