from hook import setup
import sys


def display(src, dst):
    print("load {} (where={})".format(normalize(dst), normalize(src)))


def normalize(name):
    for path in sys.path:
        name = name.replace(path, "")
    return name.lstrip("/").rsplit("/__init__.py", 1)[0].rsplit(".py", 1)[0].replace("/", ".")


setup(display)
from wsgiref.simple_server import make_server  # NOQA
