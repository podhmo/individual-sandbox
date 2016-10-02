from redbaron import RedBaron
from collections import OrderedDict


def update_funs(t0, t1):
    d = OrderedDict()
    used = set()
    for node in t1.find_all("def", recursive=False):
        d[node.name] = node

    for node in t0.find_all("def"):
        if node.name in d:
            node.arguments = d[node.name].arguments
        used.add(node.name)

    for name, node in d.items():
        if name not in used:
            t0.append(node)
    return t0


def main():
    with open("./funs.py") as rf:
        t0 = RedBaron(rf.read())

    with open("./newfuns.py") as rf:
        t1 = RedBaron(rf.read())
    return update_funs(t0, t1).dumps()


if __name__ == "__main__":
    def wrap_by(fn, *args, **kwargs):
        try:
            import sys
            from io import StringIO
            sys.stdout, original = StringIO(), sys.stdout
            result = fn(*args, **kwargs)
        finally:
            print(sys.stdout.getvalue())
            sys.stdout = original
        return result

    import autopep8
    code = wrap_by(main)
    print(autopep8.fix_code(code))
