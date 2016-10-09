import logging
import time
logger = logging.getLogger(__name__)


class Extension:
    def __init__(self):
        self.start_time = time.time()

    def elapsed_time(self):
        return time.time() - self.start_time


class LogRecordExtension(logging.LogRecord):
    extension = Extension()

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)
        self.elapsed_time = self.extension.elapsed_time()


if __name__ == "__main__":
    logging.basicConfig(level=logging.DEBUG, format="%(elapsed_time)10.10fs %(levelname)7s %(message)s")
    logging.setLogRecordFactory(LogRecordExtension)

    logger.warning("hello")
    logger.info("sleep 0.2s")
    time.sleep(0.2)
    logger.info("----------------------------------------")
    logger.info("sleep 0.4s")
    time.sleep(0.4)
    logger.info("----------------------------------------")
    logger.error("hai")
