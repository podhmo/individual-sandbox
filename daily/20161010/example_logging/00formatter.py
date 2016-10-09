import logging
from collections import defaultdict
logger = logging.getLogger(__name__)


class Extension:
    def __init__(self):
        self.c = defaultdict(int)


class LogRecordExtension(logging.LogRecord):
    extension = Extension()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.extension.c[self.msg] += 1
        self.call_count = self.extension.c[self.msg]


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(message)s[%(call_count)d]")
    logging.setLogRecordFactory(LogRecordExtension)

    logger.info("hello")
    logger.info("hello")

    logger.info("%s: hello", "foo")
    logger.info("%s: hello", "bar")
