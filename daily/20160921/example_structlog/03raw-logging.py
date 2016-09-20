# -*- coding:utf-8 -*-
import logging
import sys
logger = logging.getLogger(__name__)

"""
    %(name)s            Name of the logger (logging channel)
    %(levelno)s         Numeric logging level for the message (DEBUG, INFO,
                        WARNING, ERROR, CRITICAL)
    %(levelname)s       Text logging level for the message ("DEBUG", "INFO",
                        "WARNING", "ERROR", "CRITICAL")
    %(pathname)s        Full pathname of the source file where the logging
                        call was issued (if available)
    %(filename)s        Filename portion of pathname
    %(module)s          Module (name portion of filename)
    %(lineno)d          Source line number where the logging call was issued
                        (if available)
    %(funcName)s        Function name
    %(created)f         Time when the LogRecord was created (time.time()
                        return value)
    %(asctime)s         Textual time when the LogRecord was created
    %(msecs)d           Millisecond portion of the creation time
    %(relativeCreated)d Time in milliseconds when the LogRecord was created,
                        relative to the time the logging module was loaded
                        (typically at application startup time)
    %(thread)d          Thread ID (if available)
    %(threadName)s      Thread name (if available)
    %(process)d         Process ID (if available)
    %(message)s         The result of record.getMessage(), computed just as
                        the record is emitted
"""


fmt = "name:%(name)s	levelno:%(levelno)s	levelname:%(levelname)s	pathname:%(pathname)s	filename:%(filename)s	module:%(module)s	lineno:%(lineno)d	funcName:%(funcName)s	created:%(created)f	asctime:%(asctime)s	msecs:%(msecs)d	relativeCreated:%(relativeCreated)d	thread:%(thread)d	threadName:%(threadName)s	process:%(process)d	message:%(message)s"
handler = logging.StreamHandler(sys.stdout)
datefmt = "@%Y/%m/%d %H:%M:%S%z@"
handler.setFormatter(logging.Formatter(fmt, datefmt, '%'))
logging.basicConfig(level=logging.DEBUG, handlers=[handler])


def hello():
    logger.info("hai")
hello()
