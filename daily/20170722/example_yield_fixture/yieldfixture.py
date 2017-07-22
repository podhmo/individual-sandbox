from collections import ChainMap
import contextlib


class Context:
    def __init__(self, args=None, kwargs=None):
        self.args = args or []
        self.kwargs = kwargs or {}

    def __repr__(self):
        return "<ctx args={!r}, kwargs={!r}>".format(self.args, self.kwargs)

    def merge(self, value):
        if value is None:
            return self
        elif hasattr(value, "keys"):
            return self.__class__(self.args, ChainMap(value, self.kwargs))
        elif isinstance(value, (list, tuple)):
            return self.__class__([*self.args, *value], ChainMap({}, self.kwargs))
        else:
            args = self.args[:]
            args.append(value)
            return self.__class__(args, ChainMap({}, self.kwargs))

    def __getitem__(self, i):
        if isinstance(i, int):
            return self.args[i]
        else:
            return self.kwargs[i]

    def __setitem__(self, i, v):
        if isinstance(i, int):
            self.args[i] = v
        else:
            self.kwargs[i] = v

    def get(self, i, default=None):
        return self.kwargs.get(i, default)


def with_context(fn):
    fn._with_context = True
    return fn


def need_context(fn):
    return hasattr(fn, "_with_context")


class Marker:
    def __init__(self):
        self.fixtures = []
        self.registered = {}

    def yield_fixture(self, fixture):
        lifted = contextlib.contextmanager(fixture)
        self.fixtures.append(lifted)
        self.registered[fixture] = lifted
        return fixture

    def lift(self, f):
        return self.registered[f] if f in self.registered else f


class Runner:
    def __init__(self, marker=None):
        self.marker = marker or Marker()

    @contextlib.contextmanager
    def use_fixture(self, ctx, fixtures):
        if not fixtures:
            yield ctx
        else:
            f = fixtures[0]
            args = []
            if need_context(f):
                args.append(ctx)
            with f(*args) as val:
                with self.use_fixture(ctx.merge(val), fixtures[1:]) as ctx:
                    yield ctx

    @property
    def fixtures(self):
        return self.marker.fixtures

    @contextlib.contextmanager
    def run(self, fixtures=None, context=None):
        fixtures = [self.marker.lift(f) for f in (fixtures or self.fixtures)]
        ctx = context or Context()  # todo: scope
        with self.use_fixture(ctx, fixtures) as ctx:
            yield ctx

    def run_with(self, args):
        if callable(args):
            fn = args
            fixtures = None
            with self.run(fixtures) as ctx:
                return fn(*ctx.args, **ctx.kwargs)
        else:
            fixtures = args

            def _run_with(fn):
                with self.run(fixtures) as ctx:
                    return fn(*ctx.args, **ctx.kwargs)

            return _run_with


def create():
    marker = Marker()
    runner = Runner(marker)
    return runner.run_with, marker.yield_fixture
