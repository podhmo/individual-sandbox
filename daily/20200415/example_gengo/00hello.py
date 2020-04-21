m = object()


def hello(*, name: str) -> None:
    main = m.import_("main")
    main.Hello(name)
