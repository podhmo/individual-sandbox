from structlog import get_logger
logger = get_logger()


def hello():
    logger.info("hello", name="foo", age=20)
    return "hello"


if __name__ == "__main__":
    print(hello())
