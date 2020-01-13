import typing as t


def hello(name: str, nick_name: t.Optional[str] = None) -> None:
    print(f"hello {name}")
    if nick_name is not None:
        print(f"	{nick_name}?")


def byebye(names: t.List[str]) -> None:
    print(f"byebye {', '.join(names)}")


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(formatter_class=type('_HelpFormatter', (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter), {}))
    subparsers = parser.add_subparsers(title='subcommands', dest='subcommand')
    subparsers.required = True

    fn = hello
    sub_parser = subparsers.add_parser(fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class)
    sub_parser.add_argument('name', help='-')
    sub_parser.add_argument('--nick-name', required=False, help='-')
    sub_parser.set_defaults(subcommand=fn)

    fn = byebye  # type: ignore
    sub_parser = subparsers.add_parser(fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class)
    sub_parser.add_argument('names', nargs='*', help='-')
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()
    subcommand = params.pop('subcommand')
    return subcommand(**params)


if __name__ == '__main__':
    main()
