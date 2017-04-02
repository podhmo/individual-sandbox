from tempfile import TemporaryDirectory
from pathlib import Path


with TemporaryDirectory() as d:
    d = Path(d)
    for i in range(10):
        with d.joinpath("{:02}.txt".format(i)).open("w") as wf:
            wf.write("i: {}".format(i))

    with d.joinpath("00.txt").open() as rf:
        print(rf.read())
    with d.joinpath("01.txt").open() as rf:
        print(rf.read())
    with d.joinpath("05.txt").open() as rf:
        print(rf.read())
    with d.joinpath("07.txt").open() as rf:
        print(rf.read())

# i: 0
# i: 1
# i: 5
# i: 7
