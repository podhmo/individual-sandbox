import logging


class MultiFileHandler(logging.Handler):
    def __init__(self, handlers, formatter=None):
        super().__init__()
        self.handlers = handlers
        self.formatter = formatter
        for lv, h in handlers:
            h.setLevel(lv)
            if formatter is not None:
                h.setFormatter(formatter)

    def close(self):
        self.acquire()
        try:
            for h in self.handlers.values():
                h.close()
        finally:
            self.release()

    def handle(self, record):
        handler = self.handlers.get(record.levelno)
        if handler is not None:
            return handler.handle(record)


def setup_logger(name=__name__):
    logger = logging.getLogger(__name__)
    fmt = '%(asctime)s - %(name)s - %(levelname)s - %(message)s'
    datefmt = '%Y-%m-%d %H:%M:%S'

    formatter = logging.Formatter(fmt, datefmt)
    mapping = {
        logging.INFO: logging.FileHandler("./info.log"),
        logging.WARN: logging.FileHandler("./warn.log"),
    }
    logger.addHandler(MultiFileHandler(mapping, formatter=formatter))
    logger.setLevel(logging.DEBUG)
    return logger
