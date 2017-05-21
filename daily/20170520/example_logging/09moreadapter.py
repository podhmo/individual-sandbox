import json
import sys
import logging


class StructLogger(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        information = {
            "extra": {
                "kwargs": kwargs,
                "structual": True,
            }
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
        d.update(record.kwargs)
        return json.dumps(d)


def main():
    logger = get_logger(__name__)
    logger.info("ok", me="(o_0)")
    logging.getLogger(__name__).info("ng")


if __name__ == "__main__":
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(StructuralFormatter())
    logging.basicConfig(level=logging.INFO, handlers=[handler])
    main()
