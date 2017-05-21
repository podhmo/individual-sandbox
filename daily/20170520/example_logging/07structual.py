import json
import sys
import logging
logger = logging.getLogger(__name__)


class StructuralFormatter:
    def format(self, record):
        d = {"msg": record.msg, "me": record.me, "level": record.levelname}
        return json.dumps(vars(record))


def main():
    logger.info("ok", extra=dict(me="(o_0)"))


if __name__ == "__main__":
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(StructuralFormatter())
    logging.basicConfig(level=logging.INFO, handlers=[handler])
    main()
