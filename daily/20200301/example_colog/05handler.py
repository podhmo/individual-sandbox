import logging

logger = logging.getLogger(__name__)

mapping = {
    "TRACE": "[ trace ]",
    "DEBUG": "[ \x1b[0;36mdebug\x1b[0m ]",
    "INFO": "[  \x1b[0;32minfo\x1b[0m ]",
    "WARNING": "[  \x1b[0;33mwarn\x1b[0m ]",
    "WARN": "[  \x1b[0;33mwarn\x1b[0m ]",
    "ERROR": "\x1b[0;31m[ error ]",
    "ALERT": "\x1b[0;37;41m[ alert ]",
    "CRITICAL": "\x1b[0;37;41m[ alert ]",
}


class ColorfulHandler(logging.StreamHandler):
    terminator = "\x1b[0m\n"

    def emit(self, record: logging.LogRecord) -> None:
        record.levelname = mapping[record.levelname]
        super().emit(record)


logging.basicConfig(handlers=[ColorfulHandler()], level=logging.DEBUG)

logger.debug("hello")
logger.info("hello")
logger.warning("hello")
logger.error("hello")
logger.critical("hello")
