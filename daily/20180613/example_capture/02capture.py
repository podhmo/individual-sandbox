import os
import sys
import tempfile
import contextlib
from logging import getLogger as get_logger
logger = get_logger(__name__)


class MultiWriter:
    def __init__(self, *outs):
        self.outs = outs

    def write(self, body):
        for out in self.outs:
            out.write(body)

    def flush(self):
        for out in self.outs:
            out.flush()

    def close(self):
        for out in self.outs:
            out.close()

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        self.flush()
        self.close()


@contextlib.contextmanager
def capture(name=None, *, out=sys.stdout):
    try:
        tmpf = tempfile.NamedTemporaryFile("w", dir=".", delete=False)
        with MultiWriter(tmpf, out) as mwf:
            if os.path.exists(name):
                logger.debug("replay start %r", name)
                with open(name) as rf:
                    for line in rf:
                        mwf.write(line)
                logger.debug("replay end")
            yield mwf
    finally:
        os.rename(tmpf.name, name)


def main():
    with capture("output.txt") as out:
        print("-")
        print("hai", file=out)


if __name__ == "__main__":
    import logging
    logging.basicConfig(level=logging.DEBUG)
    main()
