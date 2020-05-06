from __future__ import annotations
from egoist.app import App


app = App()
app.include("egoist.directives.define_struct_set")


# todo: jsoniter
# todo: other framework. (e.g. go-chi, gin, echo...)


@app.define_struct_set("egoist.generate.structkit:walk")  # type: ignore
def models__models() -> None:
    from egoist.generate.structkit import runtime, structkit
    import objects

    # TODO: custom tags: e.g. json:"xxx"  -> json:"xxx" + field:"xxx"
    # def build_tag(cls: t.Type[t.Any], row: Row):
    #     name = row[0]
    #     return '`json:"{name}"`'

    # setup_build_tag(build_tag)

    with runtime.generate(structkit, classes=[objects.Person]) as m:
        m.package("models")


if __name__ == "__main__":
    app.run()
