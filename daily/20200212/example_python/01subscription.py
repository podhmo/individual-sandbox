import typing as t
import inspect
import dataclasses
from monogusa.langhelpers import reify
from functools import wraps


@dataclasses.dataclass
class Event:
    content: str


def _normalize(fn):
    spec = inspect.getfullargspec(fn)
    if len(spec.args) == 2:

        @wraps(fn)
        def _caller(ev: Event):
            return fn(ev.content, ev)

        return _caller
    elif len(spec.args) == 1:

        @wraps(fn)
        def _caller(ev: Event):
            return fn(ev.content)

        return _caller
    else:
        raise ValueError("please Callable[[body]] or Callable[[body, event]]")


class Subscription:
    @reify
    def handlers(self) -> t.List[t.Callable[[Event], t.Any]]:
        return []

    normalize = staticmethod(_normalize)

    def subscribe(self, fn, *, normalize=None):
        normalize = normalize or self.normalize
        _caller = normalize(fn)
        self.handlers.append(_caller)
        return fn

    def __call__(self, ev: Event):
        for h in self.handlers:
            h(ev)


s = Subscription()


@s.subscribe
def f(body: str, ev: Event):
    print("@", body, ev)


@s.subscribe
def g(body: str):
    print("@", body)


ev = Event("foo")
s(ev)
