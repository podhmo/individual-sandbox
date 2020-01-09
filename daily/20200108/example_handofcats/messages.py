import logging

logger = logging.getLogger(__name__)


def debug():
    logger.debug("DEBUG")


def info():
    logger.info("INFO")


def warning():
    logger.warning("WARNING")
