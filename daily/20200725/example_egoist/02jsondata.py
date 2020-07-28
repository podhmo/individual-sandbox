import typing_extensions as tx
from egoist_cli import run_func_body, printf, Shortname, as_command


class Config:
    name: str


@as_command
def hello(*, config: tx.Annotated[Config, Shortname("c")]) -> None:
    """hello message"""
    with run_func_body():
        printf("hello %s\n", config.name)
