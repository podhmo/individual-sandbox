import typing as t
import os.path
import subprocess
import logging
from dictknife import loading
from schemalint import streams
from schemalint.loader import get_loader  # todo: rename
from schemalint.formatter import get_formatter  # todo: rename
from schemalint.validator import get_validator

logger = logging.getLogger(__name__)


def run(filename: str, *, schema: t.Optional[str] = None):
    filepath = os.path.abspath(filename)
    s = streams.from_filename(filepath)

    if schema is not None:
        schemapath = os.path.abspath(schema)
        s = streams.with_schema(s, schemapath, check_schema=True)

    formatter = get_formatter(filepath, loader=s.context.loader)
    for ev in s:
        print(formatter.format(ev.error))

    print("----------------------------------------")
    subprocess.run(["cat", "-n", filepath])


if __name__ == "__main__":
    from schemalint import cli

    cli.main(run=run)
