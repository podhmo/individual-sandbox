import backoff
import logging
logger = logging.getLogger(__name__)


@backoff.on_exception(backoff.expo, (ZeroDivisionError, ), max_tries=5)
def tick(i):
    logger.info("tick %s", i)
    return 1 / 0


def run():
    tick(0)
    tick(1)


if __name__ == "__main__":
    logging.basicConfig(format="%(asctime)s %(levelname)s %(message)s", level=logging.INFO)
    run()
