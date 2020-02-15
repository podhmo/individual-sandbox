import typing as t
import re
import sys
import contextlib
from handofcats import as_command


def recv(rx: t.Pattern[str], line: str) -> None:
    m = rx.search(line)
    if m is not None:
        print("!", line)


@as_command
def run(*, filename: t.Optional[str] = None):
    with contextlib.ExitStack() as s:
        if filename is None:
            stream = sys.stdin
        else:
            stream = s.enter_context(open(filename))

        rx = re.compile("e")  # include 'e'
        for line in stream:
            line = line.rstrip()
            print("<-", line)
            recv(rx, line)
