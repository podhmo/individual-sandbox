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


fixture = Marker()


def get_fixture() -> Marker:
    global fixture
    return fixture


def get_resolver() -> Resolver:
    return Resolver(get_fixture())


def resolve_args(fn: t.Callable[..., t.Any]) -> t.List[t.Any]:
    return get_resolver().resolve_args(fn)
