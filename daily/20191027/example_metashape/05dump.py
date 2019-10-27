from prestring.python import Module
from dump import FS


def main():
    with FS() as fs:
        with fs.file("x.py") as m:
            m.stmt("x = 1")

        fs.file("y.py", code("y"))

        with m.dir("zzz"):
            with fs.file("z0.py") as m:
                m.stmt("z = 1")
            fs.file("z1.py", code("z"))

def code(name):
    m = Module()
    m.stmt("{} = 1", name)


main()
