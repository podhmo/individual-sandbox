import inspect
from functools import partial


def find_original(fn):
    while isinstance(fn, partial):
        fn = fn.func
    if inspect.isclass(fn):
        fn = fn.__init__
    return fn


import unittest  # noqa


class Tests(unittest.TestCase):
    def _callFut(self, *args, **kwargs):
        return find_original(*args, **kwargs)

    def test_it(self):
        def f(x, y, *, z):
            return (x, y, z)

        f0 = partial(f, 10)
        f1 = partial(f, z=10)
        g = partial(partial(f, 10), z=20)

        class Ob:
            def __init__(self, name, age=0):
                self.name = name
                self.age = age

        x0 = partial(Ob, "foo")
        x1 = partial(Ob, age=10)
        y = partial(partial(Ob, "foo"), age=20)

        candidates = [
            (f, f),
            (f, f0),
            (f, f1),
            (f, g),
            (Ob.__init__, Ob),
            (Ob.__init__, x0),
            (Ob.__init__, x1),
            (Ob.__init__, y),
        ]
        for expected, target in candidates:
            with self.subTest(target=target):
                got = self._callFut(target)
                self.assertEqual(got, expected)


if __name__ == "__main__":
    unittest.main()
