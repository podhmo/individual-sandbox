import logging
from monogusa.cli import run


def hello():
    logging.info("start")
    print("hello")
    logging.info("end")


if __name__ == "__main__":
    run()
