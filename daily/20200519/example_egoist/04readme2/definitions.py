from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_dir")


@app.define_dir("egoist.generators.dirkit:walk")
def output(*, source="input/hello.tmpl") -> None:
    from egoist.generators.dirkit import runtime

    with open(source) as rf:
        template = rf.read()

    for name in ["foo", "bar", "boo"]:
        with runtime.create_file(f"{name}.json", depends_on=[source]) as wf:
            print(template.format(name=name), file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
