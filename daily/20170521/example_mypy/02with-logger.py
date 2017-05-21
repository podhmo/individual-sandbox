import logging
from typing import Union

Loggable = Union[logging.Logger, logging.LoggerAdapter]


class Request:
    def __init__(self, logger: Loggable) -> None:
        self.logger = logger


logger = logging.getLogger(__name__)


def main() -> None:
    logging.basicConfig(level=logging.INFO)
    request = Request(logging.LoggerAdapter(logger, {}))
    request.logger.info("hello")


if __name__ == "__main__":
    main()
