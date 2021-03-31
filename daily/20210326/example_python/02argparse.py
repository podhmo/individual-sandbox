import typing as t


def hello(target: str):
    """description 1"""
    print("hello", target)


def byebye(target: str):
    """description 1"""
    print("byebye", target)


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        )
    )
    subparsers = parser.add_subparsers(title="subcommands", dest="subcommand")
    subparsers.required = True

    fn = hello
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.add_argument("target", help="-")
    sub_parser.set_defaults(subcommand=fn)

    fn = byebye  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.add_argument("target", help="-")
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()
    subcommand = params.pop("subcommand")
    return subcommand(**params)


if __name__ == "__main__":
    main()
