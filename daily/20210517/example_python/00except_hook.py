import sys


def p(x, y, z):
    print(f"** {x}, {y}, {z}**")

sys.excepthook = p

x
