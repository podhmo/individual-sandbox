import os
import sys
import tempfile
from functools import wraps


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


def capture(name, out):
    def decorated(fn):
        @wraps(fn)
        def _decorated(*args, **kwargs):
            try:
                tmpf = tempfile.NamedTemporaryFile("w", dir=".", delete=False)
                with MultiWriter(tmpf, out) as mwf:
                    if os.path.exists(name):
                        with open(name) as rf:
                            for line in rf:
                                mwf.write(line)
                    return fn(mwf, *args, **kwargs)
            finally:
                os.rename(tmpf.name, name)

        return _decorated

    return decorated


@capture("output.txt", sys.stdout)
def main(out):
    print("-")
    print("hai", file=out)


main()
