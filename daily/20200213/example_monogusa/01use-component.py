from defaultcomponent import Foo


def run(foo: Foo):
    print("!", foo)


if __name__ == "__main__":
    from monogusa import cli

    cli.run()
