# -*- coding:utf-8 -*-
from datetime import datetime
import sys
import json
import logging
logger = logging.getLogger(__name__)


class JSONRepr:
    def __init__(self, v, **kwargs):
        self.v = v
        self.cached = None
        kwargs["sort_keys"] = True
        if "default" not in kwargs:
            kwargs["default"] = str
        self.kwargs = kwargs

    def __str__(self):
        if self.cached is None:
            self.cached = json.dumps(self.v, **self.kwargs)
        return self.cached


class ContextualLogger(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        return msg, {
            "extra": {
                "kwargs": kwargs,
                "contextual": True,
            },
            "stack_info": kwargs.pop("stack_info", False),
            "exc_info": kwargs.pop("exc_info", False),
        }


logger = ContextualLogger(logger, {})
normalLogger = logging.getLogger("normal")


def hello():
    logger.info("hello ", myname="foo", age=20, birth=datetime(2000, 1, 1), stack_info=True)
    normalLogger.info("normal %s", "hai")
    return "hello"


class MyFormatter:
    def __init__(self, formatter=None):
        self.formatter = formatter or logging.Formatter()

    def format(self, record):
        if not getattr(record, "contextual", False):
            return self.formatter.format(record)

        kwargs = {
            "name": record.name,
            "filename": record.filename,
            "lineno": record.lineno,
            "asctime": self.formatter.formatTime(record),
            "msg": record.msg,
        }
        if record.exc_info:
            kwargs["exc_info"] = self.formatter.formatException(record.exc_info)
        kwargs.update(record.kwargs)
        return str(JSONRepr(kwargs, indent=2))


def div0():
    try:
        return 1 / 0
    except:
        logger.info("hmm..", exc_info=True)


def breaking():
    return div0()


if __name__ == "__main__":
    root = logging.getLogger()
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(MyFormatter(logging.Formatter(logging.BASIC_FORMAT)))
    root.addHandler(handler)
    root.setLevel(logging.DEBUG)

    print("----------------------------------------")
    print(hello())
    print("----------------------------------------")
    print(breaking())
    print("----------------------------------------")
    # import requests
    # requests.get("http://pod.hatenablog.com")
