# -*- coding:utf-8 -*-
from datetime import datetime
import json
import logging
logger = logging.getLogger(__name__)


class JSONRepr:
    def __init__(self, v):
        self.v = v
        self.cached = None

    def __str__(self):
        if self.cached is None:
            self.cached = json.dumps(self.v, sort_keys=True, default=str)
        return self.cached


class ContextualLogger(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        return msg, {
            "extra": {
                "kwargs": kwargs,
            },
            "stack_info": kwargs.pop("stack_info", False),
            "exc_info": kwargs.pop("exc_info", False),
        }


logger = ContextualLogger(logger, {})


def hello():
    logger.info("hello ", myname="foo", age=20, birth=datetime(2000, 1, 1), stack_info=True)
    # logger.info("hello ", stack_info=True)
    print("hello")


if __name__ == "__main__":
    fmt = "%(asctime)s %(levelname)s %(name)s %(message)s %(kwargs)s"
    logging.basicConfig(level=logging.DEBUG, format=fmt)
    hello()
