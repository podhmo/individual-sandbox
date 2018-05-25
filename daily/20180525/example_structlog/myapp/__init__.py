from mylog import get_logger
logger = get_logger(__name__)

def f():
    logger.bind(username="alice").info("hello")

