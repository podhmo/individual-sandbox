from __future__ import annotations
import typing as t
from handofcats import as_command, Config
from magicalimport import import_module
from egoist.app import create_app, SettingsDict
from egoist.scan import scan_module


@as_command(config=Config(ignore_expose=True))
def run(
    filepath: str,
    *,
    rootdir: str = "cmd/",
    here: t.Optional[str] = None,
    action: t.Literal["generate", "scan"] = "generate",
) -> None:
    m = import_module(filepath)
    here = here or m.__file__
    settings: SettingsDict = {"rootdir": rootdir, "here": here}
    app = create_app(settings)
    app.include("egoist.directives.define_cli")

    define_cli = app.define_cli("egoist.generators.clikit:walk")

    commands = scan_module(m)
    for name, fn in commands.items():
        define_cli(fn)
    app.run([action])
