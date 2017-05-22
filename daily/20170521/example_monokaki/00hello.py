from monokaki import get_logger, basic_config, INFO as INFO_LEVEL
logger = get_logger(__name__)


def hello(log):
    log.bind(age=20).info("hello")
    log.info("bye", age=20)


def main():
    hello(logger.bind(name="foo"))


if __name__ == "__main__":
    basic_config(level=INFO_LEVEL)
    main()
