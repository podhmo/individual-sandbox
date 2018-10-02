import typing as t
import sys

if t.TYPE_CHECKING:
    print("1")
    sys.exit(1)
    raise Exception("oops")
