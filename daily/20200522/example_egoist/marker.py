import typing as t
from collections import defaultdict

T = t.TypeVar("T")


class Marker(t.Generic[T]):
    pool: t.Dict[str, t.Callable[..., t.Any]]
    default_pool: t.Dict[t.Type[t.Any], t.Callable[..., t.Any]]

    def __init__(self, name: str, *, default: T) -> None:
        self.name = name
        self.pool: t.Dict[str, t.List[t.Callable[..., t.Any]]] = defaultdict(list)
        self._default = default

    def mark(
        self,
        fn: t.Callable[..., t.Any],
        *,
        name: t.Optional[str] = None,
        val: t.Optional[T] = None,
    ) -> t.Callable[..., t.Any]:
        name = name or fn.__name__
        self.pool[name].append(fn)
        val = val or self._default
        setattr(fn, f"_marked_as_{self.name}", val)
        return fn

    def __call__(self, fn: t.Callable[..., t.Any]) -> T:
        v = getattr(fn, f"_marked_as_{self.name}", self._default)  # type: T
        return v

    def is_marked(self, fn: t.Callable[..., t.Any]) -> bool:
        v = getattr(fn, f"_marked_as_{self.name}", self._default)
        return v != self._default

    def __contains__(self, name: str) -> bool:
        return name in self.pool
