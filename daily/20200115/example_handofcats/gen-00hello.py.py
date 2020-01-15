import typing as t


def hello():
    print("hello")


def bye():
    print("bye")


def main(argv: t.Optional[t.List[str]] = None) -> t.Any:
    import argparse

    parser = argparse.ArgumentParser(
        formatter_class=type(
            "_HelpFormatter",
            (argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter),
            {},
        )
    )

    # setup logging -- start
    import logging

    logging_level_choices = list(logging._nameToLevel.keys())
    parser.add_argument("--logging", choices=logging_level_choices, default=None)
    # setup logging -- end

    subparsers = parser.add_subparsers(title="subcommands", dest="subcommand")
    subparsers.required = True

    fn = hello
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    fn = bye  # type: ignore
    sub_parser = subparsers.add_parser(
        fn.__name__, help=fn.__doc__, formatter_class=parser.formatter_class
    )
    sub_parser.set_defaults(subcommand=fn)

    args = parser.parse_args(argv)
    params = vars(args).copy()

    # activate logging -- start
    import os
    import sys

    logging_level = None
    logging_format = (
        None
        or "level:%(levelname)s\tname:%(name)s\twhere:%(filename)s:%(lineno)s\trelative:%(relativeCreated)s\tmessage:%(message)s"
    )
    logging_stream = None
    if os.environ.get("DEBUG"):
        logging_level = logging.DEBUG
        print(
            "** {where}: DEBUG=1, activate logging **".format(where=__name__),
            file=sys.stderr,
        )
    if os.environ.get("LOGGING_LEVEL"):
        logging_level = logging._nameToLevel.get(os.environ["LOGGING_LEVEL"])
    if os.environ.get("LOGGING_FORMAT"):
        logging_format = os.environ["LOGGING_FORMAT"]
    if os.environ.get("LOGGING_STREAM"):
        logging_stream = getattr(sys, os.environ["LOGGING_STREAM"])
    if "logging" in params:
        level = params.pop("logging", None)
        if level is not None:
            logging_level = level
    if logging_level is not None:
        logging.basicConfig(
            level=logging_level, format=logging_format, stream=logging_stream
        )
    # activate logging -- end

    subcommand = params.pop("subcommand")
    return subcommand(**params)


if __name__ == "__main__":
    main()
