import typing as t

# see also ../../20200118/example_argparse/05updated.py


def s3(argv=None):
    def cp():
        pass

    def ls():
        pass

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

    fn = cp
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    fn = ls  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()
    subcommand = params.pop("subcommand")
    return subcommand(**params)


def sqs(argv=None):
    def list():
        pass

    def get():
        pass

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

    fn = list
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    fn = get  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()
    subcommand = params.pop("subcommand")
    return subcommand(**params)


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse
    from argparse import _

    parser = argparse.ArgumentParser(
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        ),
        add_help=False,
    )
    parser.add_argument(
        "-h", "--help", action="store_true", help=_("show this help message and exit"),
    )

    subparsers = parser.add_subparsers(title="subcommands", dest="subcommand")

    fn = s3
    sub_parser = subparsers.add_parser(
        fn.__name__,
        help=fn.__doc__,
        formatter_class=parser.formatter_class,
        add_help=False,
    )
    sub_parser.set_defaults(subcommand=fn)

    fn = sqs  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__,
        help=fn.__doc__,
        formatter_class=parser.formatter_class,
        add_help=False,
    )
    sub_parser.set_defaults(subcommand=fn)

    args, rest = parser.parse_known_args(argv)
    if args.subcommand is None and args.help:
        parser.print_help()
        parser.exit()

    if args.help:
        rest.append("-h")
    return args.subcommand(rest)


if __name__ == "__main__":
    main()
