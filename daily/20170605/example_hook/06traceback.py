import traceback


def peek(name):
    traceback.print_stack()
    raise ImportError(name)


import sys
sys.path_hooks.insert(0, peek)
import json
