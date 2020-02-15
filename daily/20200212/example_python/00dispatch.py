import inspect
import dataclasses


@dataclasses.dataclass
class Event:
    content: str


def f(body: str, ev: Event):
    print("@", body, ev)


def g(body: str):
    print("@", body)


def h(*, body: str, ev: Event):
    print("@", body, ev)


def normalize(fn):
    spec = inspect.getfullargspec(fn)
    if len(spec.args) == 2:

        def _caller(ev: Event):
            return fn(ev.content, ev)

        return _caller
    elif len(spec.args) == 1:

        def _caller(ev: Event):
            return fn(ev.content)

        return _caller
    else:
        raise ValueError("please Callable[[body]] or Callable[[body, event]]")


sig = inspect.signature(f)
print(sig, sig.parameters)
print("----------------------------------------")

sig = inspect.signature(g)
print(sig, sig.parameters)
print("----------------------------------------")

sig = inspect.signature(h)
print(sig, sig.parameters)
print("----------------------------------------")

for fn in [f, g]:
    caller = normalize(fn)
    caller(Event("foo"))
normalize(f)
normalize(g)
# normalize(h)
