import typing as t
import os.path
import subprocess
import logging
from schemalint import streams
from schemalint.loader import get_loader  # todo: rename
from schemalint.formatter import get_formatter  # todo: rename

logger = logging.getLogger(__name__)


def run(filename: str, *, schema: t.Optional[str] = None):
    filepath = os.path.abspath(filename)

    loader = get_loader(filepath)
    stream = streams.from_loader(loader)

    formatter = get_formatter(filepath, loader=loader)
    for ev in stream:
        print(formatter.format(ev.error))

    print("----------------------------------------")
    subprocess.run(["cat", "-n", filepath])



if __name__ == "__main__":
    from schemalint import cli
    cli.main(run=run)
