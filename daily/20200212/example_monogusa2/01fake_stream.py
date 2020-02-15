import logging
from monogusa.events import EventParser, subscription
from monogusa.events import Message

logger = logging.getLogger(__name__)


@subscription.subscribe(Message)
def echo(ev: Message) -> None:
    print("!", ev.content)


def read():
    import typing as t
    import sys
    import os
    import io

    def stream(default_or_io: t.Union[str, t.IO[str]]):
        if not os.isatty(sys.stdin.fileno()):
            return sys.stdin

        if isinstance(default_or_io, io.StringIO):
            o = default_or_io
        else:
            o = io.StringIO()
            o.write(default_or_io)
        if not o.getvalue().endswith("\n"):
            o.write("\n")
        o.seek(0)
        return o

    p = EventParser(sep=",")
    for line in stream("Message, hello\nMessage, byebye"):
        line = line.strip()
        if not line:
            continue
        if line.startswith("#"):
            continue

        logger.debug(f"<- %r", line)
        ev = p.parse(line)
        subscription(ev)
