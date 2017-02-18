# -*- coding:utf-8 -*-
import logging
from tinydb import TinyDB
from tinydb.storages import JSONStorage
from tinydb.middlewares import CachingMiddleware, Middleware

logger = logging.getLogger(__name__)


class LoggingMiddleware(Middleware):
    prefix = ""

    def read(self):
        data = self.storage.read()
        logger.info("%sread: %s", self.prefix, data)
        return data

    def write(self, data):
        logger.info("%swrite: %s", self.prefix, data)
        return self.storage.write(data)


class InnerLoggingMiddleware(LoggingMiddleware):
    prefix = "[INNER] "


class OuterLoggingMiddleware(LoggingMiddleware):
    prefix = "[OUNTER] "


def output(x):
    print("")
    print(x)

logging.basicConfig(level=logging.DEBUG)

db = TinyDB('store.json', storage=OuterLoggingMiddleware(CachingMiddleware(InnerLoggingMiddleware(JSONStorage))))

db.purge()
output("-")

with db as db:
    db.insert({"v": 1})
    db.insert({"v": 2})
    db.insert({"v": 3})

output(db.all())
db.insert({"v": 10})

output(db.all())
