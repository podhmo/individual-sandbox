import logging
from typing import Union, TypeVar

LoggableT = Union[logging.Logger, logging.LoggerAdapter]
T = TypeVar('T', bound="StructuralLogger")


class StructuralLoggerT(LoggableT):
    __slots__ = ()

    def bind(self, **kwargs) -> T:
        pass


class Request:
    def __init__(self, logger: StructuralLogger) -> None:
        self.logger = logger


def hello(req: Request) -> str:
    req.logger.bind(foo="hai").info("hello")
    return "hello"


logger = logging.getLogger(__name__)


def main() -> None:
    logging.basicConfig(level=logging.INFO)
    request = Request(logging.LoggerAdapter(logger, {}))
    print(hello(request))


if __name__ == "__main__":
    main()
