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


def check(s: State, name: str) -> str:
    import json

    try:
        current = getattr(s.resource, name)
    except AttributeError:
        return "unknown"

    if name not in s.storage:
        return "create"

    keys = [f.name for f in dataclasses.fields(current)]
    d = s.storage[name]
    loaded = current.__class__(**{k: d[k] for k in keys})

    # todo: cached
    if json.dumps(dataclasses.asdict(current), sort_keys=True) != json.dumps(
        dataclasses.asdict(loaded), sort_keys=True
    ):
        return "changed"
    return "nochanged"


def do_check(s: State, name: str) -> None:
    print("\t", name, check(s, name))


logging.basicConfig(level=logging.DEBUG)
storage = {}
s = State(resource, storage)
do_check(s, "foo")
print(s.foo)
print(storage)
s2 = State(resource, storage)
print(s.foo)
print("-")
do_check(s, "xxx")
do_check(s, "foo")
s2.resource.foo.name = "foo01"
do_check(s2, "foo")
