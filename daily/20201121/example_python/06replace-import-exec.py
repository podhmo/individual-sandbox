import sys
import bdb
import linecache
from types import FrameType


class _InternalBreak(Exception):
    pass


class _Internal(bdb.Bdb):
    def user_line(self, frame: FrameType):
        sys.settrace(None)

        inner_code = []
        filename = frame.f_code.co_filename
        lineno = frame.f_lineno
        print("****************************************")
        lv = 0
        max_lines = len(linecache.getlines(filename, frame.f_globals))
        while max_lines > lineno:
            source = linecache.getline(filename, lineno, frame.f_globals)
            next_lv = len(source) - len(source.lstrip(" "))
            print("\t\t-", source, end="")
            inner_code.append(source)
            if 0 < next_lv < lv:
                break
            if next_lv:
                lv = next_lv
            lineno += 1
        print("****************************************")
        raise _InternalBreak("ok")
        # linecache.clearcache()


class extract_as_code:
    def __init__(self):
        self.b = _Internal(skip=["bdb"])

    def __enter__(self):
        self.b.set_trace()

    def __exit__(self, typ, val, tb):
        sys.settrace(None)
        if typ is None or isinstance(val, _InternalBreak):
            return True


print("outer start")
with extract_as_code():
    # ここの中をASTとして取り出して、実行したい
    print("inner")  # NEVER
    print("hoi")

    def xxx():
        pass

    xxx()
print("outer end")
