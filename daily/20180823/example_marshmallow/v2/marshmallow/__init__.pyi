import typing as t

T = t.TypeVar("T", bound=t.Mapping)


class Schema(t.Generic[T]):
    def load(self, data: t.Mapping) -> t.Tuple[T, t.Mapping]:
        ...
