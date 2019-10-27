import fs
from fs.memoryfs import MemoryFS

# TODO: sync?
# TODO: logging (trace) ?


def create() -> MemoryFS:
    my_fs = fs.open_fs("mem://root")

    with my_fs.makedir("projects") as d:
        with d.open("xxx.txt", "w") as f:
            f.write("xxx")

        with d.open("yyy.txt", "w") as f:
            f.write("yyy")

        with d.open("zzz.txt", "w") as f:
            f.write("zzz")
    return my_fs


with create() as my_fs:
    print(my_fs)

    print(my_fs.listdir("/"))
    print(my_fs.listdir("/projects"))
    print(list(my_fs.glob("/**/*.txt")))
