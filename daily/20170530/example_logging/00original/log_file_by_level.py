import logging


class LoggingFilter(object):
    """特定のログレベルのみ残すフィルター"""

    def __init__(self, level):
        self.__level = level

    def filter(self, logRecord):
        # handlerへのレベル設定を外してここは== self.__levelとするのでも可のはず。
        return logRecord.levelno <= self.__level


def set_handler(loglevel, filename):
    # 共通のログフォーマット
    log_format = logging.Formatter(
        '%(asctime)s - %(name)s - %(levelname)s - %(message)s', datefmt='%Y-%m-%d %H:%M:%S'
    )
    handler = logging.FileHandler(filename)
    handler.setLevel(loglevel)
    handler.setFormatter(log_format)
    handler.addFilter(LoggingFilter(loglevel))
    logger.addHandler(handler)


logger = logging.getLogger(__name__)
# アプリで記録すべき最低限のレベルを指定。
logger.setLevel(logging.DEBUG)
set_handler(logging.WARN, './warning.log')
set_handler(logging.INFO, './info.log')
