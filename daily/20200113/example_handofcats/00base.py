import contextlib
import os
import logging
from prestring.utils import LazyFormat
from _codeobject import Module as _Module
from _fake import _FakeModule


def logging_activate(params):
    if os.environ.get("DEBUG"):
        logging_level = logging.DEBUG
        print("** {where}: DEBUG=1, activate logging **".format(where=__name__))

    if logging_level is not None:
        logging.basicConfig(level=logging_level)


def gen(m):
    os = m.import_("os")
    logging = m.import_("logging")
    logging_level = m.let("logging_level", None)

    with contextlib.suppress(Fail):
        with m.if_(os.environ.get("DEBUG")):
            logging_level = m.let("logging_level", logging.DEBUG)
            m.stmt(
                m.symbol("print")(
                    "** {where}: DEBUG=1, activate logging **".format(where=__name__)
                )
            )
    with contextlib.suppress(Fail):
        with m.if_(m._is_not_(logging_level, None)):
            m.stmt(logging.basicConfig(level=logging_level))
    return m


class Module(_Module):
    def _is_not_(self, x, y):
        return LazyFormat("{} is not {}", x, y)


class FakeModule(_FakeModule):
    def _is_not_(self, x, y):
        return x is not y

    @contextlib.contextmanager
    def if_(self, x):
        if not x:
            raise Fail()
        yield None


class Fail(Exception):
    pass


m = Module()
print(gen(m))
print(gen(FakeModule()))
