class runtime:
    @staticmethod
    def get_module():
        return object()


def run(*, name: str) -> None:
    m = runtime.get_module()
    hello = m.import_("xxx/hello")
    print(hello.Hello(name))
