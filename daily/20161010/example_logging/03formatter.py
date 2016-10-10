import logging
logger = logging.getLogger(__name__)

logging.basicConfig(level=logging.DEBUG, format="%(asctime)s %(levelname)s %(name)s :%(message)s")
logger.info("hello")
logger.info("bye")
