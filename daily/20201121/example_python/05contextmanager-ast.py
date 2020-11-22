import sys
import linecache
from types import FrameType
import bdb  # noqa
import readline  # noqa

# sys.addaudithook(partial(print, "**", file=sys.stderr))


class B(bdb.Bdb):
    def user_call(self, frame: FrameType, argument_list):
        filename = frame.f_code.co_filename
        lineno = frame.f_lineno
        source = linecache.getline(filename, lineno, frame.f_globals)
        print(f"\tCALL***{filename}:{lineno}:{source}", end="")

    def user_line(self, frame: FrameType):
        filename = frame.f_code.co_filename
        lineno = frame.f_lineno
        source = linecache.getline(filename, lineno, frame.f_globals)
        print(f"\tLINE***{filename}:{lineno}:{source}", end="")
        sys.settrace(None)

        inner_code = []
        print("****************************************")
        lv = 0
        max_lines = len(linecache.getlines(filename, frame.f_globals))
        while max_lines > lineno:
            source = linecache.getline(filename, lineno, frame.f_globals)
            next_lv = len(source) - len(source.lstrip(" "))
            if 0 < next_lv < lv:
                break
            print("\t\t-", source, end="")
            inner_code.append(source)
            if next_lv:
                lv = next_lv
            lineno += 1
        print("****************************************")
        import ast
        import textwrap

        t = ast.parse(textwrap.dedent("".join(inner_code)))
        print(ast.dump(t))
        linecache.clearcache()


class Foo:
    def __init__(self):
        self.b = b = B(skip=["bdb"])

    def __enter__(self):
        print("start")
        self.b.set_trace()

    def __exit__(self, typ, val, tb):
        print("end")
        sys.settrace(None)


def my_trace(*args, **kwargs):
    print("##", args, kwargs)


print("outer start")
with Foo():
    # ここの中をASTとして取り出して、実行したい
    print("inner")
    print("hoi")

    def xxx():
        pass

    xxx()
print("outer end")
