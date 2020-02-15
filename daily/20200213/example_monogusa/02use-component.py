from defaultcomponent import Foo


def ok(foo: Foo):
    print("!", foo)


def ng(f: Foo):
    print("!", f)


if __name__ == "__main__":
    from monogusa import cli

    cli.run()
