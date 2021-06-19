import dataclasses
import logging

logger = logging.getLogger(__name__)

# TODO: 依存関係
# TODO: bulk load
# TODO: plan and apply


@dataclasses.dataclass
class Foo:
    name: str

    def execute(self):
        import random
        import string
        import time

        return {
            "name": self.name,
            "id": "".join([random.choice(string.ascii_letters) for _ in range(16)]),
            "createdAt": time.time(),
        }


class State:
    def __init__(self, resource, storage):
        self.resource = resource
        self.storage = storage

    # TODO: bulk request
    def __getattr__(self, name):
        r = getattr(self.resource, name)
        logger.info("load : resource %r", name)

        v = self.storage.get(name)
        if v is not None:
            # TODO: update check
            return v

        logger.info("create: resource %r", name)
        v = self.storage[name] = r.execute()
        return v


class resource:
    foo = Foo(name="foo")


logging.basicConfig(level=logging.DEBUG)
storage = {}
s = State(resource, storage)
print(s.foo)
print(storage)
s2 = State(resource, storage)
print(s.foo)
