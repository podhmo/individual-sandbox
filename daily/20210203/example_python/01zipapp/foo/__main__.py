import foo
from foo.hello import hello


def main():
    app = foo.make_app(lambda environ: {"message": hello("world")})
    foo.run_app(app, 8000)


if __name__ == "__main__":
    main()
