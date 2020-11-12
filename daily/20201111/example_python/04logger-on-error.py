import sys
import logging


def foo():
    print("foo")
    bar()
    print("foo")


def bar():
    print("bar")
    boo()
    print("bar")


def boo():
    print("boo")
    raise Exception("oops")


def p(x, y):
    print("@@", x, y, file=sys.stderr)


sys.addaudithook(p)
logger = logging.getLogger(__name__)
logging.basicConfig(
    level=logging.DEBUG,
    format="%(levelname)s:%(name)s:%(message)s -- %(module)s.%(funcName)s:%(lineno)d",
)


def log(typ, val, tb):
    logger.info("hmm %r", val, exc_info=val)


sys.excepthook = log
foo()
