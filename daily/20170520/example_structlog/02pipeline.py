import time
import structlog
import logging
from collections import OrderedDict
from structlog import get_logger
from structlog import DropEvent
logger = get_logger()


class Dropper:
    def __init__(self, level):
        self.mapping = logging._nameToLevel.copy()
        self.level = self.mapping[level.upper()]

    def __call__(self, logger, method_name, event_dict):
        levelname = method_name.upper()
        level = self.mapping.get(levelname)
        event_dict["level"] = levelname
        if level is None:
            return event_dict
        if level < self.level:
            raise DropEvent
        return event_dict


def timestamper(logger, log_method, event_dict):
    event_dict["time"] = time.time()
    return event_dict


def hello():
    logger.bind(hoi="hai").debug("before")
    logger.info("hello", name="foo", age=20)
    logger.bind(hoi="hai").debug("after")
    return "hello"


def broken():
    try:
        return 1 / 0
    except Exception as e:
        logger.error(e, exc_info=True)


def make_ordered_context(*args, **kwargs):
    d = OrderedDict(*args, **kwargs)
    d["time"] = ""
    d["level"] = ""
    d["msg"] = ""
    d["caller"] = ""
    d["source"] = ""
    return d


class LTSVRenderer(structlog.processors.KeyValueRenderer):
    def __call__(self, _, __, event_dict):
        items = self._ordered_items(event_dict)
        return '\t'.join("{k}:{v}".format(k=k, v=v) for k, v in items)


if __name__ == "__main__":
    structlog.configure(
        context_class=make_ordered_context,
        processors=[
            Dropper("INFO"),
            timestamper,
            LTSVRenderer(),
        ],
    )
    print("@", hello())
    print("@", broken())
