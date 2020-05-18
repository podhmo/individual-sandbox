from __future__ import annotations
import typing as t
from egoist.app import App


app = App(settings={"here": __file__, "rootdir": ""})
app.include("egoist.directives.define_struct_set")


@app.define_struct_set("egoist.generators.structkit:walk")  # type: ignore
def models__models() -> None:
    from egoist.generators.structkit import runtime, structkit
    import objects

    @runtime.set_metadata_handler
    def metadata_handler(
        cls: t.Type[t.Any], *, name: str, info: t.Any, metadata: runtime.Metadata
    ) -> None:
        """with form"""
        metadata["tags"] = {"json": [name.rstrip("_")], "form": [name.rstrip("_")]}

    with runtime.generate(structkit, classes=[objects.Person]) as m:
        m.package("models")


if __name__ == "__main__":
    app.run()
