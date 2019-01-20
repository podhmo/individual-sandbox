import os
import logging
logger = logging.getLogger(__name__)


def main():
    logger.debug("debug message1")
    logger.info("hello")
    logger.debug("debug message2")


# 丁寧にmain()を定義したり__name__のifを付けない場合もある
if __name__ == "__main__":
    logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
    main()
