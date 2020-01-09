from monogusa.dependencies import scan_module, ignore


def hello(*, name: str) -> None:
    print(f"hello {name}")


def byebye(*, name: str) -> None:
    print(f"byebye {name}")


@ignore
def main():
    from monogusa.web.codegen import _codeobject as codeobject
    from monogusa.web.codegen._codeobject import Module
    from monogusa.cli import runtime
    from handofcats.injector import Injector

    m = Module()

    # hmm
    m.stmt("from codegen import hello, byebye")

    with m.def_("create_parser"):
        argparse = m.import_("argparse")
        driver = runtime.Driver(parser=m.let("parser", argparse.ArgumentParser()))
        driver.subparsers = m.let("subparsers", driver.subparsers)
        if driver.subparsers.required:
            m.stmt("{}.required = True", driver.subparsers)
        m.sep()

        scanned = runtime.scan_module()
        for fn in scanned.commands:
            sub_parser = m.let(
                "sub_parser",
                driver.subparsers.add_parser(
                    fn.__name__, help=runtime._get_summary(fn)
                ),
            )

            # NOTE: positional arguments are treated as component
            Injector(fn).inject(sub_parser, ignore_arguments=True, callback=m.stmt)
            m.stmt("{}.set_defaults(subcommand={})", sub_parser, fn.__name__)
            sub_parser.set_defaults(subcommand=fn)
            m.sep()

        m.return_(driver.parser)

    with m.def_("main"):
        parser = m.let("parser", codeobject.Symbol("create_parser")())
        args = m.let("args", parser.parse_args())
        params = m.let("params", codeobject.Symbol("vars")(args))
        action = m.let("action", params.pop("subcommand"))
        m.sep()
        m.stmt("# TODO positionals")
        m.sep()
        m.stmt("{}(**params)", action)

    with m.if_("__name__ == '__main__'"):
        m.stmt("main()")
    print(m)


if __name__ == "__main__":
    main()
