import linecache
import inspect
import logging
from pycomment.parse import parse_string, node_name, PyTreeVisitor


class Visitor(PyTreeVisitor):
    def __init__(self, lineno):
        self.lineno = lineno
        self.r = []

    # todo: performance
    def visit(self, node):
        if node.get_lineno() == self.lineno:
            self.r.append(node)
            return
        super().visit(node)


def rfind(t, lineno):
    visitor = Visitor(lineno)
    visitor.visit(t)
    r = [lineno]

    history = set([lineno])
    seen = set()
    for node in visitor.r:
        target = node
        while target:
            if id(target) in seen:
                break
            if node_name(target) in ("funcdef", "classdef"):
                i = target.get_lineno()
                if i not in history:
                    history.add(i)
                    r.append(target.get_lineno())
            elif target.parent is None:
                break
            seen.add(id(target))
            target = target.parent
    return reversed(r)


def run(filename, *, lineno: int):
    print("----------------------------------------")
    with open(filename) as rf:
        t = parse_string(rf.read())
    for i in rfind(t, lineno):
        print(f"{i:03d}: {linecache.getline(filename, i)}", end="")


run("./template.py", lineno=2)
run("./template.py", lineno=7)
run(inspect.getsourcefile(logging), lineno=1000)

import argparse

run(inspect.getsourcefile(argparse), lineno=1000)
run(inspect.getsourcefile(argparse), lineno=2000)
