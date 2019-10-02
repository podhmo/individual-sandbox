import typing as t
import logging
import types
from typing_extensions import Protocol

logger = logging.getLogger(__name__)

_SysExcInfoType = t.Union[
    t.Tuple[type, BaseException, t.Optional[types.TracebackType]],
    t.Tuple[None, None, None],
]
_ExcInfoType = t.Union[None, bool, _SysExcInfoType, BaseException]


class LoggerProtocol(Protocol):
    def info(
        self,
        msg: t.Any,
        *args: t.Any,
        exc_info: _ExcInfoType = ...,
        stack_info: bool = ...,
        extra: t.Optional[t.Dict[str, t.Any]] = ...,
        **kwargs: t.Any,
    ) -> None:
        ...


class WLogger(logging.LoggerAdapter):
    def process(
        self, msg: str, kwargs: t.MutableMapping[str, t.Any]
    ) -> t.Tuple[str, t.MutableMapping[str, t.Any]]:
        return f"Wrap [{msg}]", kwargs


def use(l: LoggerProtocol) -> None:
    l.info("hello")


def main() -> None:
    logging.basicConfig(
        level=logging.INFO, format=logging.BASIC_FORMAT + " (%(funcName)s)"
    )
    use(logger)
    use(WLogger(logger, {}))


if __name__ == "__main__":
    main()
