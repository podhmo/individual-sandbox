import logging

logger = logging.getLogger(__name__)


class Adapter(logging.LoggerAdapter):
    def process(self, msg, kwargs):
        print(msg, kwargs)
        return msg, kwargs


adapter = Adapter(logger, {"hoi": "h@i"})
logging.basicConfig(level=logging.DEBUG)

adapter.info("hello: name: %s", "foo")
