"""
loggingの設定

LTSVで出力。pythonのloggingライブラリとは互換性が全くない

time:%Y-%m-%dT%H:%M:%S.%f%z
level:debug,info,warning,error
msg:<message of text>
caller:$<file path>:<lineno>
source:<logger name>

エラーの時にstackにstack traceを保存
"""

import logging
import traceback
import sys
import os.path
from collections import OrderedDict
from datetime import datetime

import structlog

DEBUG = logging.DEBUG
INFO = logging.INFO
WARNING = logging.WARNING
ERROR = logging.ERROR
CRITICAL = logging.CRITICAL
NAME_TO_LEVEL = logging._nameToLevel


def processor_basic_information(logger, name, event_dict):
    event_dict["time"] = datetime.now().strftime("%Y-%m-%dT%H:%M:%S.%f%z")
    event_dict["level"] = name
    return event_dict


def processor_find_caller_information(depth=4):
    def processor(logger, name, event_dict):
        f = sys._getframe(depth)
        # On some versions of IronPython, currentframe() returns None if
        # IronPython isn't run with -X:Frames.
        if f is not None:
            f = f.f_back
        rv = "(unknown file)", 0, "(unknown function)", None
        while hasattr(f, "f_code"):
            co = f.f_code
            sinfo = None
            filename = os.path.abspath(os.path.normcase(co.co_filename))
            rv = (filename, f.f_lineno, co.co_name, sinfo)
            break

        # add to dict
        event_dict["caller"] = "{}:{}".format(rv[0].replace(os.environ.get("HOME", "HOME"), "~"), rv[1])
        # event_dict["fn"] = rv[2]  # function name
        return event_dict

    return processor


def processor_oneline_on_exception(src):
    def processor(logger, name, event_dict):
        exc = event_dict.pop(src, None)
        if exc:
            event_dict[src] = repr(traceback.format_exc(chain=False, limit=1))
        return event_dict

    return processor


def processor_stack_on_exception(src, dst):
    def processor(logger, name, event_dict):
        exc = event_dict.pop(src, None) or event_dict.get("exc_info")
        exc_info = structlog.processors._figure_out_exc_info(exc)
        if exc_info:
            exc = structlog.processors._format_exception(exc_info)
        if exc:
            event_dict[dst] = str(exc)
        return event_dict

    return processor


def processor_rename_key_value(src, dst):
    def processor(logger, name, event_dict):
        if src in event_dict:
            event_dict[dst] = event_dict.pop(src, None)
        return event_dict

    return processor


class LTSVRenderer(structlog.processors.KeyValueRenderer):
    def __call__(self, _, __, event_dict):
        items = self._ordered_items(event_dict)
        return '\t'.join("{k}:{v}".format(k=k, v=v) for k, v in items)


_loggers = []
_level = logging.INFO


def get_logger(name, *args, **kwargs):
    logger = structlog.get_logger(name).bind(source=name)
    _loggers.append(logger)
    return logger


getLogger = get_logger  # backward comapatibility of python stdlib


def processor_filter_by_level(logger, name, event_dict):
    global _level
    level = NAME_TO_LEVEL.get(name.upper()) or 0
    if level >= _level:
        return event_dict
    else:
        raise structlog.DropEvent


def _get_default_renderer():
    if os.environ.get("LOGTYPE", "").lower() == "ltsv":
        return LTSVRenderer()
    else:
        return structlog.processors.JSONRenderer()


# yapf: disable
DEFAULT_PROCESSORS = [
    processor_filter_by_level,
    processor_find_caller_information(depth=2),
    processor_basic_information,
    processor_oneline_on_exception("oneline_exception"),
    processor_stack_on_exception("exception", "stack"),
    processor_rename_key_value("event", "msg"),
    structlog.processors.StackInfoRenderer(),
    _get_default_renderer(),
]
# yapf: enable


def make_ordered_context(*args, **kwargs):
    d = OrderedDict(*args, **kwargs)
    d["time"] = ""
    d["level"] = ""
    d["msg"] = ""
    d["caller"] = ""
    d["source"] = ""
    return d

# structlogは、configure()を呼ぶ前にget_logger()されたloggerに関してはconfigが適用されない(ひどい)
def setup(*args, **kwargs):
    kwargs["context_class"] = kwargs.pop("context_class", make_ordered_context)
    kwargs["processors"] = kwargs.pop("processors", DEFAULT_PROCESSORS)
    structlog.configure(*args, **kwargs)


def set_loglevel(level):
    global _level
    if isinstance(level, str):
        level = NAME_TO_LEVEL[level.upper()]
    _level = level
