import contextlib
import sys
import typing as t
from io import StringIO


@contextlib.contextmanager
def with_prefix(prefix: str, *, out: t.IO = sys.stdout) -> t.Optional[str]:
    o = StringIO()
    with contextlib.redirect_stdout(o):
        yield

    o.seek(0)
    for line in o:
        out.write(prefix)
        out.write(line)
    if hasattr(out, "getvalue"):
        return out.getvalue()


with with_prefix("# "):
    print("foo")
    print("foo")
