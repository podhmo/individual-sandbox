from . import hookimpl


@hookimpl
def greeting(msg: str) -> str:
    return f"hello {msg}"
