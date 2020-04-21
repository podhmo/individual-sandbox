import typing as t


def hello(name: str) -> str:
    return f"Hello {name}"


def run(*, name: str) -> None:
    print(hello(name))


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(
        prog=run.__name__,
        description=run.__doc__,
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        ),
    )
    parser.print_usage = parser.print_help  # type: ignore
    parser.add_argument("--name", required=True, help="-")
    args = parser.parse_args(argv)
    params = vars(args).copy()
    return run(**params)


if __name__ == "__main__":
    main()
