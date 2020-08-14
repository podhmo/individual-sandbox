import json
import sys


def walk(d, pkg, indent=0):
    print("  " * indent, pkg)
    for subpkg in d[pkg]:
        walk(d, subpkg, indent=indent + 1)


def graph(d, pkg):
    import queue
    from prestring.text import Module

    m = Module()
    q = queue.Queue()
    q.put(pkg)
    seen = set()
    m.stmt("digraph {")
    with m.scope():
        while not q.empty():
            pkg = q.get()
            if pkg in seen:
                continue
            seen.add(pkg)
            m.stmt(f"// {pkg}")
            for next_pkg in d[pkg]:
                m.stmt(f"{pkg.replace('/', '_')} -> {next_pkg.replace('/', '_')}")
                q.put(next_pkg)
            m.sep()
    m.stmt("}")
    print(m)


load_path = sys.argv[1]
pkg = sys.argv[2]
with open(load_path) as rf:
    d = json.load(rf)

# walk(d, pkg)
graph(d, pkg)
