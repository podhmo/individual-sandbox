import typing as t

def hello(*, name="world") -> str:
    return f"hello, {name}"


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(prog=hello.__name__, description=hello.__doc__, formatter_class=type('_HelpFormatter', (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter), {}))
    parser.print_usage = parser.print_help  # type: ignore
    parser.add_argument('--name', required=False, default='world', help='-')
    args = parser.parse_args(argv)
    params = vars(args).copy()
    return hello(**params)


if __name__ == '__main__':
    main()
