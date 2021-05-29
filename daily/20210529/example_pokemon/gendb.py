import typing as t
import os
import logging

logger = logging.getLogger(__name__)


def run(
    *, input_file: str, output_file: str = "data.db", db_name: str = "data"
) -> None:
    import pandas as pd
    from sqlalchemy import create_engine

    logger.info("load %s", input_file)
    df = pd.read_csv(input_file)

    logger.info("save %s", input_file)
    engine = create_engine(f"sqlite:///{output_file}", echo=False)
    df.to_sql(db_name, engine, if_exists="replace")


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
    parser.add_argument("--input-file", required=True, help="-")
    parser.add_argument("--output-file", required=False, default="data.db", help="-")
    parser.add_argument("--db-name", required=False, default="data", help="-")
    args = parser.parse_args(argv)
    params = vars(args).copy()
    action = run
    if bool(os.getenv("FAKE_CALL")):
        from inspect import getcallargs
        from functools import partial

        action = partial(getcallargs, action)  # type: ignore
    return action(**params)


if __name__ == "__main__":
    main()
