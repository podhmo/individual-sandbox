import typing as t
import logging


logger = logging.getLogger(__name__)
LoggerType = t.Union[logging.Logger, logging.LoggerAdapter]


class WLogger(logging.LoggerAdapter):
    def process(
        self, msg: str, kwargs: t.MutableMapping[str, t.Any]
    ) -> t.Tuple[str, t.MutableMapping[str, t.Any]]:
        return f"Wrap [{msg}]", kwargs


def use(l: LoggerType) -> None:
    l.info("hello")
    if isinstance(l, logging.LoggerAdapter):
        use2(l)


def use2(l: logging.LoggerAdapter) -> None:
    l.info("hai")


def main() -> None:
    logging.basicConfig(
        level=logging.INFO, format=logging.BASIC_FORMAT + " (%(funcName)s)"
    )
    use(logger)
    use(WLogger(logger, {}))


if __name__ == "__main__":
    main()
