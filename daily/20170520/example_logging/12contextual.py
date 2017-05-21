import time
import json
import sys
import logging
from collections import ChainMap


class StructLogger(logging.LoggerAdapter):
    def bind(self, **kwargs):
        return self.__class__(self.logger, ChainMap(kwargs, self.extra))

    def process(self, msg, kwargs):
        information = {
            "extra": {
                "kwargs": ChainMap(kwargs, self.extra),
                "structual": True,
            },
            "stack_info": kwargs.pop("stack_info", False),
            "exc_info": kwargs.pop("exc_info", False),
        }
        return msg, information


def get_logger(name):
    return StructLogger(logging.getLogger(name), {})


class StructuralFormatter:
    def __init__(self, formatter=None):
        self.formatter = formatter or logging.Formatter(logging.BASIC_FORMAT)

    def format(self, record):
        if not getattr(record, "structual", False):
            return self.formatter.format(record)
        d = {"msg": record.msg, "level": record.levelname}
        if record.exc_info:
            d["stack"] = self.formatter.formatException(record.exc_info)
        if record.stack_info:
            d["stack"] = self.formatter.formatStack(record.stack_info)
        d.update(record.kwargs)
        return json.dumps(d)


def f(logger, x):
    logger = logger.bind(time=time.time()).bind(f="on f")
    return g(logger, x)


def g(logger, x):
    logger = logger.bind(g="on g")
    return h(logger, x)


def h(logger, x):
    logger = logger.info("h is called")
    return x * x


def main():
    logger = get_logger(__name__)
    f(logger, 10)
    logger.info("end")


if __name__ == "__main__":
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(StructuralFormatter())
    logging.basicConfig(level=logging.INFO, handlers=[handler])
    main()
