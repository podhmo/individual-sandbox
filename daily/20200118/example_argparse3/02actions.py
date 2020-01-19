from handofcats import as_subcommand


@as_subcommand
def foo(target: str):
    import heavy_module

    print("foo")
    heavy_module.hello(target)


@as_subcommand
def bar(target: str):
    import heavy_module

    print("bar")
    heavy_module.hello(target)


if __name__ == "__main__":
    import sys
    import itertools

    sep = "-"
    itr = iter(sys.argv[1:])
    while True:
        argv = list(itertools.takewhile(lambda x: x != sep, itr))
        if len(argv) == 0:
            break
        as_subcommand.run(argv=argv)
