import logging
import traceback
logger = logging.getLogger(__name__)


class Handler(logging.StreamHandler):
    def emit(self, record):
        traceback.print_stack()
        super().emit(record)


def f():
    g()


def g():
    logger.info("hello")


logging.basicConfig(level=logging.INFO, handlers=[Handler()])
f()
