import logging
import contextlib
logger = logging.getLogger(__name__)


class Counter:
    def __init__(self, i=0):
        self.i = i

    def inc(self):
        i = self.i
        self.i += 1
        return i


class CellManager:
    def __init__(self, counter=None):
        self.counter = counter or Counter()

    @contextlib.contextmanager
    def code(self):
        logger.debug("code: {}".format(self.counter.inc()))
        yield

    @contextlib.contextmanager
    def markdown(self):
        raise NotImplemented


def main():
    cm = CellManager()

    with cm.code():
        x = 1
        y = 2
        x + y

logging.basicConfig(level=logging.DEBUG)
main()
# with cm.markdown("markdown") as m:
#     m.write("""
#     hello this is foo
#     """)
