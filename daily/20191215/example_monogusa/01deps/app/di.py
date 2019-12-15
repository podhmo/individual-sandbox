import typing as t
import inspect


class Marker:
    def __init__(self):
        self.pool = {}

    def __call__(self, fn):
        self.pool[fn.__name__] = fn
        return fn

    def __contains__(self, name: str) -> bool:
        return name in self.pool


class Resolver:
    def __init__(self, marker: Marker):
        self.marker = marker
        self.registry: t.Dict[str, t.Any] = {}

    def resolve_args(
        self, fn: t.Callable[..., t.Any], *, strict: bool = True
    ) -> t.List[t.Any]:
        argspec = inspect.getfullargspec(fn)
        # XXX: for `from __future__ import annotations`
        annotations = t.get_type_hints(fn)
        assert len(argspec.annotations) == len(annotations)
        argspec.annotations.update(annotations)

        g = self.marker.pool
        if not argspec.args and fn.__name__ in g:
            return []

        args = []
        for name in argspec.args:
            if name in self.registry:
                val = self.registry[name]
                args.append(val)
                if strict:
                    assert isinstance(val, argspec.annotations[name])
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

            if strict:
                assert isinstance(val, argspec.annotations[name])

        return args


component = Marker()


def get_component_marker() -> Marker:
    global component
    return component


def get_resolver() -> Resolver:
    return Resolver(get_component_marker())


def resolve_args(fn: t.Callable[..., t.Any]) -> t.List[t.Any]:
    return get_resolver().resolve_args(fn)
