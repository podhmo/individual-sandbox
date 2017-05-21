import logging
logger = logging.getLogger(__name__)


def main():
    logger.info("error is occured errno: %d", 404)


if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO, format="%(asctime)s " + logging.BASIC_FORMAT)
    main()
