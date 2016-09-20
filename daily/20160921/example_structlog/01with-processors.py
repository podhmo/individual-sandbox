import structlog
import logging
import calendar
import time


def timestamper(logger, log_method, event_dict):
    event_dict["timestamp"] = calendar.timegm(time.gmtime())
    return event_dict


is_dropped = False


def dropper(logger, log_method, event_dict):
    global is_dropped
    print("\t@", log_method, event_dict, "@")
    if is_dropped:
        raise structlog.DropEvent
    return event_dict

logger = structlog.get_logger()
logger = structlog.wrap_logger(logger, processors=[timestamper, dropper])
logger = logger.bind(user="anonymous", some_key=23)

logging.basicConfig(level=logging.DEBUG)
logger.info("hello")
logger.info("hello")
is_dropped = True
logger.info("hello2")
is_dropped = False
logger.info("bye")
