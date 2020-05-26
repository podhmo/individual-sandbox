import typing as t
from functools import partial, update_wrapper
from egoist.app import App

AnyFunction = t.Callable[..., t.Any]
T = t.TypeVar("T")


class Directive:
    def __init__(
        self, define_fn: AnyFunction, *, name: str, requires: t.List[str]
    ) -> None:
        self.define_fn = define_fn
        self.seen: bool = False
        self.name = name
        self.requires = requires

    @property
    def __call__(self) -> AnyFunction:
        return self.define_fn

    def register(self, app: App, *args, **kwargs) -> AnyFunction:
        def _register(target_fn: AnyFunction) -> AnyFunction:
            if not self.seen:
                self.seen = True
            self.define_fn(app, target_fn, *args, **kwargs)
            return target_fn

        return _register

    def includeme(self, app: App) -> None:
        """callback for app.include()"""

        # for information used by describe()
        directive = partial(self.register)
        update_wrapper(directive, self.define_fn)
        app.add_directive(self.name, directive)

        def _include() -> None:
            if self.seen or not self.requires:
                return

            seen = app._aggressive_import_cache
            for path in self.requires:
                if path in seen:
                    continue
                app.include(path)

        app.action(self.name, _include)


def directive(
    directive_fn: t.Optional[t.Callable[..., t.Any]] = None,
    *,
    name: str,
    requires: t.List[str],
):
    def _directive(directive_fn: t.Callable[..., t.Any]):
        ob = Directive(directive_fn, name=name, requires=requires)
        update_wrapper(ob, directive_fn)
        return ob

    return _directive
