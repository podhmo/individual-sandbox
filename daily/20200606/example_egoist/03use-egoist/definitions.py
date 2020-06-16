from __future__ import annotations
from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_file")


@app.define_file("egoist.generators.filekit:walk")
def noop() -> None:
    from egoist.generators.filekit import runtime

    with runtime.create_file() as wf:
        print("noop", file=wf)


@app.define_file("egoist.generators.filekit:walk", suffix=".formatted.go")
@app.include_when("egoist.ext.gofmtrpc")
def hello__hello(*, filename: str = "./hello/hello.go") -> None:
    from egoist.generators.filekit import runtime
    from egoist.ext.gofmtrpc import gofmt

    with runtime.create_file() as wf:
        with open(filename) as rf:
            code = rf.read()

        formatted_code = gofmt(code)
        print(formatted_code, file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
