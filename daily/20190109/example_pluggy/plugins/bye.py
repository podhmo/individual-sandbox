from . import hookimpl


@hookimpl
def greeting(msg: str) -> str:
    return f"bye {msg}"
