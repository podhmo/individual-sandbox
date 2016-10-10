import logging
logger = logging.getLogger(__name__)

logging.basicConfig(level=logging.DEBUG)
logger.info("hello")
logger.info("bye")

# INFO:__main__:hello
# INFO:__main__:bye

