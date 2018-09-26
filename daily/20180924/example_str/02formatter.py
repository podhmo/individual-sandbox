import logging
from collections import defaultdict
logging.basicConfig(level=logging.INFO)


class MockRecord:
    def getMessage(self):
        return self.__dict__["message"]


m = MockRecord()
idmap = defaultdict(lambda: len(idmap))
m.__dict__ = idmap
print(logging.root.handlers[0].formatter._style.format(m))
print(m.__dict__)
