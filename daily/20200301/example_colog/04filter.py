import logging

logger = logging.getLogger(__name__)


class Filter(logging.Filter):
    def filter(self, record: logging.LogRecord) -> bool:
        print(vars(record))
        record.levelname = record.levelname.lower()
        return True


logging.basicConfig(level=logging.DEBUG)
for h in logging.root.handlers:
    h.addFilter(Filter())

logger.info("hello: name: %s", "foo")
