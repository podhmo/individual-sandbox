from io import StringIO, TextIOBase


class Tee(TextIOBase):
    def __init__(self, source):
        self.source = source
        self.buf = StringIO()

    def __iter__(self):
        for line in self.source:
            self.buf.write(line)
            yield line

    def __enter__(self):
        return self

    def __exit__(self, typ, val, tb):
        self.close()

    def close(self):
        self.source.close()
        self.buf = None


with open("00source.txt") as rf:
    for line in rf:
        print("@", line)

print("----------------------------------------")
with Tee(open("00source.txt")) as rf:
    for line in rf:
        print("@", line)
print(rf.buf)
