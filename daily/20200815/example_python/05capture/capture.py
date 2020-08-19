from __future__ import annotations
import typing as t

CallT = t.TypeVar("CallT", bound=t.Callable[..., t.Any])


class Capture:
    def __init__(self, fn: CallT) -> None:
        self.fn = fn
        self.called = False

    def __call__(
        self, *args: t.Tuple[t.Any, ...], **kwargs: t.Dict[str, t.Any]
    ) -> t.Any:
        self.called = True
        return self.fn(*args, **kwargs)
