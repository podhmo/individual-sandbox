from __future__ import annotations
import typing as t
import inspect
from handofcats import as_command, Config
from magicalimport import import_module
from egoist.app import create_app, SettingsDict
from egoist.scan import scan_module


@as_command(config=Config(ignore_expose=True))
def run(
    filepath: str,
    *,
    pkg: str = "model",
    rootdir: str = "",
    here: t.Optional[str] = None,
    action: t.Literal["generate", "scan"] = "generate",
) -> None:
    m = import_module(filepath)
    here = here or m.__file__
    settings: SettingsDict = {"rootdir": rootdir, "here": here}
    app = create_app(settings)
    app.include("egoist.directives.define_struct_set")

    define_struct_set = app.define_struct_set("egoist.generators.structkit:walk")

    scanned = scan_module(m, is_ignored=lambda x: not inspect.isclass(x))
    classes = list(scanned.values())

    def models():
        from egoist.generators.structkit import runtime, structkit

        with runtime.generate(structkit, classes=classes) as m:
            m.package(pkg)

    define_struct_set(models)
    app.run([action])
