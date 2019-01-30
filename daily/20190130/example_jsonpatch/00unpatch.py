from collections import namedtuple
cmd = namedtuple("cmd", "op, x, y")


def unpatch(src, dst):
    return list(walk(src, dst))


def walk(src, dst):
    # xxx: src and dst is None
    if hasattr(src, "keys"):
        if not hasattr(dst, "keys"):
            dst = {"": dst}  # xxx
        return _walk_dict(src, dst)
    elif isinstance(src, (list, tuple)):
        if not isinstance(dst, (list, tuple)):
            dst = [dst]  # xxx
        return _walk_list(src, dst)
    else:
        return _walk_atom(src, dst)


def _walk_dict(src, dst):
    r = {}
    for k, v in src.items():
        if k in dst:
            r[k] = walk(v, dst[k])
        else:
            r[k] = [cmd("remove", x=v, y=None)]
    for k, v in dst.items():
        if k in r:
            continue
        r[k] = [cmd("add", x=v, y=None)]
    return list(r.items())


def _walk_list(src, dst):
    r = {}
    n = min(len(src), len(dst))
    for i in range(n):
        r[str(i)] = _walk_list(src[i], dst[i])

    if n == len(src):
        for i in range(n, len(dst)):
            r[str(i)] = [cmd(op="add", x=dst[i])]
    else:
        for i in range(n, len(src)):
            r[str(i)] = [cmd(op="add", x=src[i])]


def _walk_atom(src, dst):
    if src is None:
        return [cmd(op="add", x=dst)]
    elif dst is None:
        return [cmd(op="remove", x=src)]
    elif src != dst:
        return [cmd(op="replace", x=src, y=dst)]
    else:
        return []


import unittest  # noqa


class Tests(unittest.TestCase):
    def _callFUT(self, src, dst):
        return unpatch(src, dst)

    def test00(self):
        src = {"name": "foo"}
        dst = {"name": "foo"}
        print(src, dst, "@", self._callFUT(src, dst))

    def test01(self):
        src = {"name": "foo"}
        dst = {"name": "bar"}
        print(src, dst, "@", self._callFUT(src, dst))

    def test02(self):
        src = {"name": "foo"}
        dst = {}
        print(src, dst, "@", self._callFUT(src, dst))

    def test03(self):
        src = {}
        dst = {"name": "bar"}
        print(src, dst, "@", self._callFUT(src, dst))

    def test04(self):
        src = {"point0": {"value": 10}}
        dst = {"point1": {"value": 10}}
        print(src, dst, "@", self._callFUT(src, dst))


if __name__ == "__main__":
    unittest.main()
