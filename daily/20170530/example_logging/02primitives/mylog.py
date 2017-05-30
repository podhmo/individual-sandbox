import logging


class SelfLevelOnlyWrapper:
    def __init__(self, handler, lv):
        self.handler = handler
        self.lv = lv
        self.handler.setLevel(lv)

    def __getattr__(self, name):
        return getattr(self.handler, name)

    def handle(self, record):
        if record.levelno == self.lv:
            self.handler.handle(record)


logger = logging.getLogger(__name__)
formatter = logging.Formatter(
    '%(asctime)s - %(name)s - %(levelname)s - %(message)s', datefmt='%Y-%m-%d %H:%M:%S'
)

whandler = logging.FileHandler("./warning.log")
whandler.setFormatter(formatter)

ihandler = logging.FileHandler("./info.log")
ihandler.setFormatter(formatter)

logging.basicConfig(
    level=logging.DEBUG,
    handlers=[
        SelfLevelOnlyWrapper(whandler, logging.WARNING),
        SelfLevelOnlyWrapper(ihandler, logging.INFO)
    ]
)
