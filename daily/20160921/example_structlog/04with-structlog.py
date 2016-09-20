# 無理

# # -*- coding:utf-8 -*-
# import structlog
# import logging
# import sys
# import calendar
# import time


# def timestamper(logger, log_method, event_dict):
#     print(event_dict, logger)
#     print("-")
#     event_dict["timestamp"] = calendar.timegm(time.gmtime())
#     return event_dict

# logger = structlog.get_logger("hmm")
# logger = structlog.wrap_logger(logger, processors=[timestamper])
# logger = logger.bind(user="anonymous", some_key=23)

# fmt = "name:%(name)s	levelno:%(levelno)s	levelname:%(levelname)s	pathname:%(pathname)s	filename:%(filename)s	module:%(module)s	lineno:%(lineno)d	funcName:%(funcName)s	created:%(created)f	asctime:%(asctime)s	msecs:%(msecs)d	relativeCreated:%(relativeCreated)d	thread:%(thread)d	threadName:%(threadName)s	process:%(process)d	message:%(message)s"
# handler = logging.StreamHandler(sys.stdout)
# datefmt = "@%Y/%m/%d %H:%M:%S%z@"
# handler.setFormatter(logging.Formatter(fmt, datefmt, '%'))
# logging.basicConfig(level=logging.DEBUG, handlers=[handler])
# logger.addHandler(handler)


# def hello():
#     logger.info("hai")
# hello()
