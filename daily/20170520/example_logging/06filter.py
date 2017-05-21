import sys
import logging
logger = logging.getLogger(__name__)


class HostInjector(logging.Filter):
    def __init__(self, name="", host=None):
        super().__init__(name)
        self.host = host

    def filter(self, record):
        record.host = self.host
        return True


def main():
    logger.info("ok")


if __name__ == "__main__":
    fmt = "%(asctime)s: host=%(host)s level=%(levelname)s %(filename)s:%(lineno)s -- %(message)s"
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(logging.Formatter(fmt))
    handler.addFilter(HostInjector(host="127.0.0.1:4444"))
    logging.basicConfig(level=logging.INFO, handlers=[handler])
    main()
