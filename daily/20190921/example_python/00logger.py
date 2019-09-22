import logging
from dictknife.langhelpers import reify

logger = logging.getLogger(__name__)


class WrappedLogger(logging.LoggerAdapter):
    @reify
    def history(self):
        return []

    def process(self, msg, kwargs):
        self.history.append((msg, kwargs))
        return msg, kwargs


def main():
    logging.basicConfig(
        level=logging.INFO, format=logging.BASIC_FORMAT + "[%(funcName)s(%(lineno)s)]"
    )

    wlogger = WrappedLogger(logger, {})
    wlogger.info("hello")
    print(wlogger.history)


main()
