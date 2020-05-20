from egoist.app import create_app, SettingsDict, parse_args

settings: SettingsDict = {"rootdir": "", "here": __file__}
app = create_app(settings)

app.include("egoist.directives.define_dir")
app.include("foo.gen")

if __name__ == "__main__":
    for argv in parse_args(sep="-"):
        app.run(argv)
