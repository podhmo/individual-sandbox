from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_dir")


@app.define_sqs("egoist.generators.sqskit:walk", infra_dir="infra", go_dir="sqs")
def output(*, source="input/hello.tmpl") -> None:
    from egoist.generators.dirkit import runtime

    with runtime.create_infra_file(f"xxx.tf") as wf:
        print("xxx.tf", file=wf)
    with runtime.create_go_file(f"xxx.go") as wf:
        print("xxx.go", file=wf)


if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
