import typing as t


def list() -> None:
    for line in ["foo", "bar", "boo"]:
        print(line)


def get(*, i: int = None) -> None:
    print(["foo", "bar", "boo"][i])


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        )
    )
    parser.print_usage = parser.print_help  # type: ignore
    subparsers = parser.add_subparsers(title="subcommands", dest="subcommand")
    subparsers.required = True

    fn = list
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    fn = get  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.add_argument("-i", required=False, type=int, help="-", default=0)
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()
    subcommand = params.pop("subcommand")
    return subcommand(**params)


if __name__ == "__main__":
    main()
