import logging
import json
import threading
from collections import OrderedDict, ChainMap
_lock = threading.RLock()


def _acquireLock():
    _lock.acquire()


def _releaseLock():
    _lock.release()


class StructLogger(logging.LoggerAdapter):
    def bind(self, **kwargs):
        return self.__class__(self.logger, ChainMap(kwargs, self.extra))

    def process(self, msg, kwargs):
        return msg, {
            "extra": {
                "kwargs": ChainMap(kwargs, self.extra),
                "structual": True,
            },
            "stack_info": kwargs.pop("stack_info", False),
            "exc_info": kwargs.pop("exc_info", False),
        }


def jsonrender(d):
    return str(json.dumps(d, indent=2))


def ltsvrender(d):
    return '\t'.join("{k}:{v}".format(k=k, v=str(v).replace("\n", "\\n")) for k, v in d.items())


class Formatter:
    def __init__(self, formatter=None, render=jsonrender):
        self.formatter = formatter or logging.Formatter()
        self.render = render

    def format(self, record):
        if not getattr(record, "structual", False):
            return self.formatter.format(record)

        kwargs = OrderedDict()
        kwargs["time"] = self.formatter.formatTime(record)
        kwargs["level"] = record.levelname
        kwargs["meg"] = record.msg
        kwargs["caller"] = "{}:{}".format(record.pathname, record.lineno)
        kwargs["source"] = record.name
        if record.exc_info:
            kwargs["stack"] = self.formatter.formatException(record.exc_info)
        if record.stack_info:
            kwargs["stack"] = self.formatter.formatStack(record.stack_info)
        kwargs.update(record.kwargs)
        return self.render(kwargs)


def getLogger(*args, **kwargs):
    raw = logging.getLogger(*args, **kwargs)
    return StructLogger(raw, {})


def basicConfig(level, stream=None, formatter=None, format=logging.BASIC_FORMAT, render=jsonrender):
    _acquireLock()
    try:
        root = logging.getLogger()
        if len(root.handlers) > 0:
            return
        handler = logging.StreamHandler(stream)
        formatter = formatter or logging.Formatter(format)
        handler.setFormatter(Formatter(formatter, render=render))
        root.addHandler(handler)
        root.setLevel(level)
    finally:
        _releaseLock()
