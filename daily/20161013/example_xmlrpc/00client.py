# -*- coding:utf-8 -*-
import logging
from xmlrpc import client


logger = logging.getLogger(__name__)


def call(s, name, *args, **kwargs):
    logger.info("->: %s args=%s kwargs=%s", name, args, kwargs)
    f = getattr(s, name)
    val = f(*args, **kwargs)
    logger.info("<- %s", val)
    return val


class Caller:
    def __init__(self, s):
        self.s = s

    def __getattr__(self, name):
        return lambda *args, **kwargs: call(self.s, name, *args, **kwargs)


def make_app(port):
    c = Caller(client.ServerProxy("http://localhost:{}".format(port)))
    logging.basicConfig(level=logging.DEBUG)
    return c


def iter_app(c):
    yield c.pow(2, 3)
    yield c.pow(2, -3)
    yield c.atan(1) * 4


def run_app(app):
    list(iter_app(app))

if __name__ == "__main__":
    run_app(make_app(4444))
