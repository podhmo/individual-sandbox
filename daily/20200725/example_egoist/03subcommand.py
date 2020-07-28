import typing_extensions as tx
from egoist_cli import run_func_body, printf, Shortname, as_subcommand


class Config:
    name: str


@as_subcommand
class App:
    def hello(self, *, config: tx.Annotated[Config, Shortname("c")]) -> None:
        """hello message"""
        with run_func_body():
            printf("hello %s\n", config.name)

    def byebye(self, *, config: tx.Annotated[Config, Shortname("c")]) -> None:
        """byebye message"""
        with run_func_body():
            printf("byebye %s\n", config.name)
