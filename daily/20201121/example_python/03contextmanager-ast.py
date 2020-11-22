import sys
from functools import partial
import readline  # noqa

# sys.addaudithook(partial(print, "**", file=sys.stderr))


class Foo:
    def __enter__(self):
        print("start")
        import pdb  # noqa

        pdb = pdb.Pdb(skip=["gdb"])
        # pdb.set_trace(sys._getframe().f_back)

    def __exit__(self, typ, val, tb):
        print("end")


print("outer")
foo = Foo()
# breakpoint()
with foo:
    # ここの中をASTとして取り出して、実行したい
    print("inner")
