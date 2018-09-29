import logging
from semver import make_semver, max_satisfying
# logging.basicConfig(level=logging.DEBUG)

print(make_semver('4.1.3.0', loose=True))
print(max_satisfying(['3.1.1', '4.1.1.0', '4.1.2.0', '4.1.3.0'], '>= 4.1.0 <5.0.0', loose=True))
print(max_satisfying(['3.1.1', '4.1.2.0', '4.1.1.0', '4.1.3.0'], '^4.1', loose=True))
