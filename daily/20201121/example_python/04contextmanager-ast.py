import sys
from functools import partial
import pdb  # noqa
import readline  # noqa

sys.addaudithook(partial(print, "**", file=sys.stderr))


class Foo:
    def __enter__(self):
        print("start")

    def __exit__(self, typ, val, tb):
        print("end")


def my_trace(*args, **kwargs):
    print("##", args, kwargs)


# sys.settrace(my_trace)
import bdb

b = bdb.Tdb()
b.quitting = False
b.botframe = None
b.stopframe = None
print("outer start")
foo = Foo()
sys.settrace(b.trace_dispatch)
b.set_step()
with foo:
    # ここの中をASTとして取り出して、実行したい
    print("inner")
    sys.settrace(None)
print("outer end")
