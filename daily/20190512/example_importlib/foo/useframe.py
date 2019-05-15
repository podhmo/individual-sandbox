import sys


def use(level=2):
    print(sys._getframe(level).f_globals["__name__"])


def use2(level=2):
    use(level=level)
