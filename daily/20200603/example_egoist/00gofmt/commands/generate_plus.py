from __future__ import annotations
import typing as t
from functools import partial
from egoist.app import App
from egoist.types import AnyFunction

if t.TYPE_CHECKING:
    from argparse import ArgumentParser

import subprocess


def generate_plus(app: App) -> None:
    from egoist.commands.generate import generate
    p = subprocess.Popen(["go", "run", "github.com/podhmo/egoist/gofmtrpc/cmd/gofmtrpc"])
    generate(app)
    
    print("do something")


def setup(app: App, sub_parser: ArgumentParser, fn: AnyFunction) -> None:
    sub_parser.set_defaults(subcommand=partial(fn, app))


def includeme(app: App) -> None:
    app.include("egoist.directives.add_subcommand")
    app.add_subcommand(setup, fn=generate_plus)
