import typing as t
import inspect


class Marker:
    def __init__(self):
        self.registry = {}

    def __call__(self, fn):
        self.registry[fn.__name__] = fn
        return fn

    def __contains__(self, name: str) -> bool:
        return name in self.registry


class Resolver:
    def __init__(self, marker: Marker):
        self.marker = marker
        self.registry: t.Dict[str, t.Any] = {}

    def resolve_args(self, fn: t.Callable[..., t.Any]) -> t.List[t.Any]:
        argspec = inspect.getfullargspec(fn)
        g = self.marker.registry

        if not argspec.args and fn.__name__ in g:
            return []

        args = []
        for name in argspec.args:
            if name in self.registry:
                args.append(self.registry[name])
                continue

            if name not in g:
                raise ValueError(
                    f"fixture ({name} : {argspec.annotations.get(name)}) is not found"
                )

            fixture_factory = g[name]
            fixture_args = self.resolve_args(fixture_factory)
            val = fixture_factory(*fixture_args)

            self.registry[name] = val
            args.append(val)

        return args


class Engine:
    def dispose(self):
        pass


fixture = Marker()


@fixture
def database_url() -> str:
    return "sqlite:///test.db"


@fixture
def engine(database_url: str) -> Engine:
    return Engine()


def init_db(engine: Engine, *, name: str, name2: str = "hmm"):
    """init tables"""
    engine.dispose()
    print("ok")


resolver = Resolver(fixture)
args = resolver.resolve_args(init_db)
print(args)
# print(typing.get_type_hints(init_db))
# print(inspect.getfullargspec(init_db))
# print(inspect.signature(init_db))
