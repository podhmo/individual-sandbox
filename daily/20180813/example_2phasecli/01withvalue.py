import typing as t
import inspect
from functools import partial


def find_original_with_arguments(fn):
    args = ()
    kwargs = {}
    if isinstance(fn, partial):
        args = fn.args
        kwargs = fn.keywords
        fn = fn.func
    if inspect.isclass(fn):
        fn = fn.__init__
    return fn, args, kwargs


# def find_original_with_arguments(fn):
#     args_list = []
#     kwargs_list = []
#     while isinstance(fn, partial):
#         args_list.append(fn.args)
#         kwargs_list.append(fn.keywords)
#         fn = fn.func
#     if inspect.isclass(fn):
#         fn = fn.__init__
#     args = tuple(itertools.chain.from_iterable(reversed(args_list)))
#     kwargs = ChainMap(*reversed(kwargs_list))
#     return fn, args, kwargs

import unittest  # noqa


class Tests(unittest.TestCase):
    def _callFut(self, *args, **kwargs):
        return find_original_with_arguments(*args, **kwargs)

    def test_it(self):
        def f(
            x: int,
            y: int,
            *,
            z: int,
            i: t.Optional[int] = None,
            j: t.Optional[int] = None,
        ):
            return (x, y, z, i, j)

        f0 = partial(f, 10)
        f1 = partial(f, z=10)
        g0 = partial(partial(f, 10), z=20)
        g1 = partial(partial(f, 10), 20)
        h0 = partial(partial(partial(f, 10), z=20), i=0, j=1, k=2)
        h1 = partial(partial(partial(f, 10), 20, z=-20), i=0, z=10)

        class Ob:
            def __init__(self, name: str, age: t.Optional[int] = None):
                self.name = name
                self.age = age

        x0 = partial(Ob, "foo")
        x1 = partial(Ob, age=10)
        y = partial(partial(Ob, "foo"), age=20)

        candidates = [
            (f, f),
            (f, f0),
            (f, f1),
            (f, g0),
            (f, g1),
            (f, h0),
            (f, h1),
            (Ob.__init__, Ob),
            (Ob.__init__, x0),
            (Ob.__init__, x1),
            (Ob.__init__, y),
        ]
        for expected, target in candidates:
            with self.subTest(target=target):
                got, args, kwargs = self._callFut(target)
                print(got, args, kwargs)
                self.assertEqual(got, expected)


if __name__ == "__main__":
    unittest.main()
