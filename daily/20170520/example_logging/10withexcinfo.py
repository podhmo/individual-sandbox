import json
import sys
import logging


class StructLogger(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        information = {
            "extra": {
                "kwargs": kwargs,
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


def main():
    logger = get_logger(__name__)
    logger.info("ok", me="(o_0)", stack_info=True)
    logging.getLogger(__name__).info("ng", stack_info=True)


if __name__ == "__main__":
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(StructuralFormatter())
    logging.basicConfig(level=logging.INFO, handlers=[handler])
    main()
