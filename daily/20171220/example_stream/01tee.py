from io import BytesIO, RawIOBase


class Tee(RawIOBase):
    def __init__(self, source):
        self.source = source
        self.buf = BytesIO()

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

    @property
    def closed(self):
        return self.source.closed

    def read(self, size=None):
        b = self.source.read(size)
        self.buf.write(b)
        return b


with open("00source.txt", "rb") as rf:
    print("@", rf.read(4096))
    print("@", rf.read(4096))

print("----------------------------------------")
with Tee(open("00source.txt", "rb")) as rf:
    print("@", rf.read(4096))
    print("@", rf.read(4096))
print(rf.buf)
