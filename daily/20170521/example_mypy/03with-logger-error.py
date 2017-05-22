import logging
from typing import Union

Loggable = Union[logging.Logger, logging.LoggerAdapter]


class Request:
    def __init__(self, logger: Loggable) -> None:
        self.logger = logger


def hello(logger: Loggable) -> str:
    logger.info("hello")
    return "hello"


def hello2(req: Request) -> str:
    req.logger.bind(foo="hai").info("hello")
    return "hello"


logger = logging.getLogger(__name__)


def main() -> None:
    logging.basicConfig(level=logging.INFO)
    request = Request(logging.LoggerAdapter(logger, {}))
    print(hello(request))
    print(hello2(request))


if __name__ == "__main__":
    main()
