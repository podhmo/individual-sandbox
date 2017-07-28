# -*- coding:utf-8 -*-
import logging
import contextlib
logger = logging.getLogger(__name__)


@contextlib.contextmanager
def code():
    yield


"""
# section

this is markdown text
next line
"""

# this is code
1 + 2

with code():
    x = 10
    y = 20
    (x, y)

"""
# section2

this is markdown text2
"""

x * x
x * y

with code():
    y * y
