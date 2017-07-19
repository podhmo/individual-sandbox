# -*- coding:utf-8 -*-
import logging
logger = logging.getLogger("sample")


def foo():
    bar()


def bar():
    logger.info("bar", stack_info=True)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO)
    foo()
