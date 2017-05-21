# -*- coding:utf-8 -*-
import logging
logger = logging.getLogger(__name__)


def hello() -> str:
    logger.info("hello")
    return "hello"


def main() -> None:
    logging.basicConfig(level=logging.INFO)
    hello()


if __name__ == "__main__":
    main()
