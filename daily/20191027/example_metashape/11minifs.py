from io import StringIO
import logging

from prestring.minifs import MiniFS, Option

logging.basicConfig(level=logging.DEBUG)

option = Option(root="11minifs", cleanup=True, fake=True)
with MiniFS(default_factory=StringIO, writer=option.writer) as fs:
    with fs.open("projects/x.txt", "w") as wf:
        print("hello x", file=wf)
        print("bye x", file=wf)

    with fs.open("projects/y.txt", "w") as wf:
        print("hello y", file=wf)
        print("bye y", file=wf)
