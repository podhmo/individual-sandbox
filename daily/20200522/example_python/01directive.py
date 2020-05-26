from egoist.app import App
from new_directive import directive, AnyFunction


@directive(name="define_foo", requires=["egoist.commands.describe"])
def define_foo(app: App, fn: AnyFunction, name: str) -> AnyFunction:
    """*** define foo ***"""
    print(app, fn, "foo", name)

    def _register():
        print("!")

    app.registry.generators[name].append(fn)
    app.action(fn.__name__, _register)
    return fn


if __name__ == "__main__":
    app = App()
    app.include("__main__.define_foo")

    @app.define_foo("xxx")
    def yyy():
        print("yyy")

    @app.define_foo("xxx")
    def zzz():
        print("zzz")

    print("s")
    app.commit()
    yyy()
    zzz()
    print("e")

    from egoist.commands.describe import describe

    describe(app)
