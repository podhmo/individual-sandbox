from monogusa.events import EventParser, subscription
from monogusa.events import Message


@subscription.subscribe(Message)
def echo(ev: Message) -> None:
    print("!", ev.content)


def read():
    import sys

    p = EventParser(sep=",")
    for line in sys.stdin:
        ev = p.parse(line)
        subscription(ev)
