from mylogger import getLogger
logger = getLogger(__name__)


def broken():
    try:
        return 1 / 0
    except:
        logger.error("broken", exc_info=True)
