from mylogger import getLogger
logger = getLogger(__name__)


def hello():
    logger.debug("before", position="hello")
    _logger = logger.bind(name="foo", age=20)
    _logger.info("hello")
    logger.debug("after", position="hello")
    return "hello"
