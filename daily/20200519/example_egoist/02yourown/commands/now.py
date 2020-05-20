from __future__ import annotations
import typing as t
from functools import partial
from egoist.app import App
from egoist.types import AnyFunction

if t.TYPE_CHECKING:
    from argparse import ArgumentParser


def now(app: App, *, format: str,) -> None:
    from datetime import datetime

    now = datetime.now()
    print(now.strftime(format))


def setup(app: App, sub_parser: ArgumentParser, fn: AnyFunction) -> None:
    sub_parser.add_argument("--format", default="%Y-%m-%dT%H:%M:%S%z")
    sub_parser.set_defaults(subcommand=partial(fn, app))


def includeme(app: App) -> None:
    app.include("egoist.directives.add_subcommand")
    app.add_subcommand(setup, fn=now)
