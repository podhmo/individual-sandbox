import logging
from semver import max_satisfying

v1 = "1.1.1.1"
v2 = "1.1.1.2"

logging.basicConfig(
    level=logging.DEBUG, format=logging.BASIC_FORMAT + "%(funcName)s:%(lineno)d"
)
print(max_satisfying([v1, v2], "^1", loose=True))
