import sys
import contextlib


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


with open("output.txt", "w") as wf:
    with MultiWriter(wf, sys.stdout) as mwf:
        with contextlib.redirect_stdout(mwf):
            print("hai")
