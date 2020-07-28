import typing_extensions as tx
from egoist_cli import run_func_body, printf, Description, as_command


@as_command
def hello(*, name: tx.Annotated[str, Description("greeting target name")]) -> None:
    """hello message"""
    with run_func_body():
        printf("hello %s\n", name)


@as_command
def byebye(*, name: str) -> None:
    """byebye message"""
    with run_func_body():
        printf("hello %s\n", name)
