import typing as t
import logging


logger = logging.getLogger(__name__)


class WLogger(logging.LoggerAdapter):
    def process(
        self, msg: str, kwargs: t.MutableMapping[str, t.Any]
    ) -> t.Tuple[str, t.MutableMapping[str, t.Any]]:
        return f"Wrap [{msg}]", kwargs


def use(l: logging.Logger) -> None:
    l.info("hello")


def main() -> None:
    logging.basicConfig(
        level=logging.INFO, format=logging.BASIC_FORMAT + " (%(funcName)s)"
    )
    use(logger)
    use(WLogger(logger, {}))


if __name__ == "__main__":
    main()
