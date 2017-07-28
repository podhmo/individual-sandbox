# -*- coding:utf-8 -*-

import logging
logger = logging.getLogger(__name__)
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


class Person:
    def __init__(self, name, age):
        self.name = name
        self.age = age

    def say(self, message):
        return "{self.name}({self.age}): {message}".format(self=self, message=message)


p = Person("foo", 20)
p.say("hello")

with code():
    p.say("bye")
