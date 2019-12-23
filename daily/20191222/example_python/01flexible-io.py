import typing as t
import sys
import json
from functools import partial
import contextlib
import traceback
from io import StringIO


def append_text(o: t.IO[str], text: t.AnyStr, *, type_="stdout") -> int:
    if text == "\n":
        return 0
    data = {"type": type_, "text": text.split("\n")}
    n = o.write(json.dumps(data))
    m = o.write("\n")
    return n + m


class LDJSONWriter:
    def __init__(self, o: t.IO[str], *, type_="stdout"):
        self.type_ = type_
        self.write = partial(append_text, o, type_=type_)


def run():
    o = StringIO()
    with contextlib.redirect_stdout(LDJSONWriter(o)):
        with contextlib.redirect_stderr(LDJSONWriter(o, type_="stderr")):
            try:
                print("hoi")
                print("hai")
                print("hmm", file=sys.stderr)
                1 / 0
            except Exception:
                tb = traceback.format_exc()
                print(tb, file=sys.stderr)
    print(o.getvalue())


def run2():
    o = StringIO()
    with contextlib.redirect_stdout(LDJSONWriter(o)):
        with contextlib.redirect_stderr(LDJSONWriter(o, type_="stderr")):
            print("")
            print("\n")
            print("\n\n")
    print(o.getvalue())


run()
run2()
