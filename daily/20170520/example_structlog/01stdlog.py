import logging
import sys
import structlog
from structlog import get_logger

logger = get_logger()


def hello():
    logger.info("hello", name="foo", age=20)
    return "hello"


def broken():
    try:
        return 1 / 0
    except Exception as e:
        logger.error(e, exc_info=True)


if __name__ == "__main__":
    logging.basicConfig(
        format="%(message)s %(asctime)s %(levelname)s)",
        stream=sys.stdout,
        level=logging.INFO,
    )
    structlog.configure(
        logger_factory=structlog.stdlib.LoggerFactory(),
    )
    print(hello())
    print(broken())
