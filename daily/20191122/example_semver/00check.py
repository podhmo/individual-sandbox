gimport logging
from semver import max_satisfying

v0 = "1.1.1"
v1 = "1.1.1.1"
v2 = "1.1.1.2"
v3 = "1.1.1-1"

logging.basicConfig(
    level=logging.DEBUG, format=logging.BASIC_FORMAT + "%(funcName)s:%(lineno)d"
)
print(max_satisfying([v0, v1, v2, v3], "^1", loose=True))
print(max_satisfying([v1, v2], "^1", loose=True))
print(max_satisfying([v1, v2, v3], "^1", loose=True))
