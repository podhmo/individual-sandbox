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
    def format(self, record):
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
