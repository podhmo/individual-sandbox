import dataclasses
import typing as t
from capturedmock import Act, Line

import sys
from capturedmock import CapturedMock, compile, scan, squash
import logging
import contextlib

logger = logging.getLogger(__name__)


def group(history: t.List[Act]) -> t.List[Line]:
    """ grouping tokens

    - trim <return-value> node
    - group by parent_id == id check
    """
    r = []
    chunk = []
    prev = None
    replace_map: t.Dict[int, int] = {}

    for act in history:
        if prev is None:
            chunk.append(act)
            prev = act
            continue

        if act.parent_id in replace_map:
            act = dataclasses.replace(act, parent_id=replace_map[act.parent_id])

        if act.name == "<return-value>":
            pass
        elif prev.id == act.parent_id and prev.name == "<return-value>":
            replace_map[act.parent_id] = prev.parent_id
            act = dataclasses.replace(act, parent_id=prev.parent_id)
            chunk.append(act)
        else:
            r.append(Line(lineno=len(r), args=chunk))
            chunk = []
            chunk.append(act)
        prev = act
    if chunk:
        r.append(Line(lineno=len(r), args=chunk))

    # debug
    for line in r:
        prev = line.args[0]
        for act in line.args[1:]:
            assert act.parent_id == prev.id, (prev, act)
            prev = act
    return r


def arrange(grouped: t.List[Line]) -> t.List[Line]:
    """ arange tokens (grouped)

    - correct as sentence
    - check as assign or not
    """
    seen = {}
    for line in grouped:
        for x in line.args:
            seen[x.id] = line
        print("@", line)

    for line in grouped:
        if line.args[0].parent_id in seen:
            breakpoint()
            print("hoi")
    pass


@contextlib.contextmanager
def use():
    print("----------------------------------------", file=sys.stderr)
    x = CapturedMock()
    yield x
    squashed = squash(x.history)
    for e in squashed:
        logger.debug("squash: %s", e)
    print("------", file=sys.stderr)
    grouped = group(squashed)
    for e in grouped:
        logger.debug("group: %s", e)
    print("------", file=sys.stderr)
    arranged = arrange(grouped)
    for e in arranged:
        logger.debug("arrange: %s", e)
    print("------", file=sys.stderr)
    # scanned = scan(squashed)
    # for e in scanned:
    #     logger.debug("scan: %s", e)
    # print("------", file=sys.stderr)
    # for code in compile(scanned):
    #     logger.info("code: %s", code)
    # print("----------------------------------------", file=sys.stderr)


logging.basicConfig(level=logging.DEBUG)

# with use() as x:
#     parser = x.ArgumentParser()
#     parser.add_argument("-x")

# with use() as x:
#     parser = x.ArgumentParser()
#     parser.add_argument("-x")
#     parser.add_argument("-y")

# with use() as x:
#     parser = x.ArgumentParser()
#     parser.add_argument("-x").add_argument("-z")

# with use() as x:
#     x.foo(10)
#     x.foo(10).bar(20)
#     x.foo(10)

with use() as argparse:
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(required=True, title="actions")

    foo_parser = subparsers.add_parser("foo")
    foo_parser.add_argument("-x")
    foo_parser.add_argument("-y")
    foo_parser.set_defaults(action="foo")

    bar_parser = subparsers.add_parser("bar")
    bar_parser.add_argument("-z")
    bar_parser.set_defaults(action="bar")
