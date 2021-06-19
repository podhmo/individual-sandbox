import dataclasses
import logging
import typing_extensions

logger = logging.getLogger(__name__)

# TODO: 依存関係
# TODO: bulk load
# TODO: plan and apply


@typing_extensions.runtime_checkable
class Executable(typing_extensions.Protocol):
    def execute(self) -> None:
        ...


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


def dict_to_object(d):
    from types import SimpleNamespace

    return SimpleNamespace(**{k: v for k, v in d.items() if isinstance(v, Executable)})


def run():
    foo = Foo(name="foo")
    foo.id = foo.xxx.id  # planでこんなコードが書けて欲しい
    return dict_to_object(locals())


print(run())
