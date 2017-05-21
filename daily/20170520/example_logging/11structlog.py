import time
from structlog import get_logger


def f(logger, x):
    logger = logger.bind(time=time.time()).bind(f="on f")
    return g(logger, x)


def g(logger, x):
    logger = logger.bind(g="on g")
    return h(logger, x)


def h(logger, x):
    logger = logger.info("h is called")
    return x * x


if __name__ == "__main__":
    logger = get_logger(__name__)
    f(logger, 10)
    logger.info("end")
