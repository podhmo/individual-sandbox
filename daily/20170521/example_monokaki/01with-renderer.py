from monokaki import get_logger, basic_config, INFO as INFO_LEVEL
from collections import OrderedDict
logger = get_logger(__name__)


def ltsv_renderer(d, record, formatter):
    c = OrderedDict()
    c["time"] = formatter.formatTime(record)
    c["level"] = record.levelname
    c["msg"] = record.msg
    c["where"] = "{}:{}({})".format(record.filename, record.lineno, record.funcName)
    c.update(sorted(record.kwargs.items()))
    if "stack" in d:
        c["stack"] = d["stack"].replace("\n", "\\n")
    return "\t".join("{}:{}".format(k, v) for k, v in c.items())


def hello(log):
    log.bind(age=20).info("hello")
    log.info("bye", age=20)
    try:
        1 / 0
    except:
        logger.info("1/0", exc_info=True)


def main():
    hello(logger.bind(name="foo"))


if __name__ == "__main__":
    basic_config(level=INFO_LEVEL, renderer=ltsv_renderer)
    main()
