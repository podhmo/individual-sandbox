# -*- coding:utf-8 -*-
import logging
logger = logging.getLogger("sample")


def foo():
    bar()


def bar():
    logger.info("bar")
    1 / 0


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    try:
        foo()
    except:
        logger.info("hmm", exc_info=True)
