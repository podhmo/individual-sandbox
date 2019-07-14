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


def run(filename, *, lineno: int, n: int = 2, show_lineno: bool = False):
    print("----------------------------------------")
    t = parse_string("".join(linecache.getlines(filename)))

    seen = []
    list_of_target_lineno = list(rfind(t, lineno))
    for i, target_lineno in enumerate(list_of_target_lineno):
        if i == 0:
            start_lineno = target_lineno
        elif i == len(list_of_target_lineno) - 1:
            start_lineno = max(1, target_lineno - n)
        else:
            start_lineno = target_lineno

        if i == len(list_of_target_lineno) - 1:
            end_lineno = target_lineno + 1
        else:
            end_lineno = min(
                len(linecache.getlines(filename)) + 1, target_lineno + n + 1
            )

        if seen and start_lineno - seen[-1] > 1:
            if show_lineno:
                print(f"...")
            else:
                print("# ...")

        for lineno in range(start_lineno, end_lineno):
            if seen and lineno <= seen[-1]:
                continue
            seen.append(lineno)
            if show_lineno:
                print(f"{lineno:03d}: {linecache.getline(filename, lineno)}", end="")
            else:
                print(linecache.getline(filename, lineno), end="")


run("./template.py", lineno=2)
run("./template.py", lineno=7)
run(inspect.getsourcefile(logging), lineno=1000)
run(inspect.getsourcefile(logging), lineno=1009)
import argparse

run(inspect.getsourcefile(argparse), lineno=2000)
run(inspect.getsourcefile(argparse), lineno=1900)
run(inspect.getsourcefile(argparse), lineno=1900, show_lineno=True)
