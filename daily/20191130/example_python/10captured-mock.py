from capturedmock import CapturedMock, compile, scan, squash
import logging
import contextlib

logger = logging.getLogger(__name__)


@contextlib.contextmanager
def use(x):
    yield x
    scanned = scan(squash(x))
    for line in scanned:
        logger.debug(line)
    for code in compile(scanned):
        logger.info(code)


logging.basicConfig(level=logging.INFO)

with use(CapturedMock()) as x:
    x.y.z(10)

print("-")

with use(CapturedMock()) as x:
    x.y
    x.z(10)
print("-")

with use(CapturedMock()) as x:
    y = x.y
    y.z0.m(20)
    y.z1.m(30)

print("-")
with use(CapturedMock()) as x:
    y = x.y
    y.f(20)
    y.f(30)
