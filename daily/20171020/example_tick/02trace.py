import traceback


def f():
    g()


def g():
    h()


def h():
    traceback.print_stack()


f()
