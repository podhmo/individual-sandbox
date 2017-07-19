# -*- coding:utf-8 -*-
import logging
import traceback
logger = logging.getLogger("sample")


def foo():
    bar()


def bar():
    logger.info("bar")
    logger.info("tb: %s", "".join(traceback.format_stack()))


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    foo()
