from handofcats import as_command


def hello(name, *, message: str = "hello world"):
    """greeting message"""
    print(f"{name}: {message}")


@as_command
def main():
    import inspect
    from prestring.python import Module
    from prestring.python.transform import transform_string

    m = Module()
    source = inspect.getsource(hello)
    m = transform_string(source, m=m)
    print(m)
