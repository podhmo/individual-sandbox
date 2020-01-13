import os
import contextlib
import logging
from _fake import _FakeModule, Fail
from _codeobject import Module


def logging_activate(params):
    if os.environ.get("DEBUG"):
        logging_level = logging.DEBUG
        print("** {where}: DEBUG=1, activate logging **".format(where=__name__))

    if logging_level is not None:
        logging.basicConfig(level=logging_level)


def emit_logging_activate(m, *, logging_level=None):
    print_ = m.symbol(print)
    __name___ = m.symbol(__name__)

    # import os
    os = m.import_("os")
    # import logging
    logging = m.import_("logging")

    # if os.environ.get("DEBUG")
    with contextlib.suppress(Fail):
        with m.if_(os.environ.get("DEBUG")):

            # logging_level = logging.DEBUG
            logging_level = m.let("logging_level", logging.DEBUG)

            # print("** {where}: DEBUG=1, activate logging **".format(where=__name___))
            m.stmt(
                print_(
                    m.format_(
                        "** {where}: DEBUG=1, activate logging **", where=__name___
                    )
                )
            )

    # if logging_level is not None:
    with contextlib.suppress(Fail):
        with m.if_(m.is_not_(logging_level, None)):

            # logging.basicConfig(level=logging_level)
            m.stmt(logging.basicConfig(level=logging_level))
    return m


m = Module()
m = emit_logging_activate(m)
print(m)

print("----------------------------------------")

m = _FakeModule()
emit_logging_activate(m)

print("----------------------------------------")

os.environ["DEBUG"] = "1"

m = _FakeModule()
emit_logging_activate(m)

logging.info("hoi")

