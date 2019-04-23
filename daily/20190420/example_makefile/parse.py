import typing as t
import re
import shlex
import itertools
import sys
import argparse


Node = t.Dict[str, t.Any]  # todo type


class PushBackIterator:
    def __init__(self, itr) -> None:
        self.itr = iter(itr)

    def pushback(self, x: str) -> None:
        self.itr = itertools.chain([x], self.itr)

    def __next__(self) -> str:
        return next(self.itr)

    def __iter__(self) -> t.Iterator[str]:
        return self


def _scan(itr: t.Iterator[str]) -> t.Iterator[str]:
    buf = []
    for line in itr:
        line = line.rstrip("\n ")
        if line.endswith("\\"):
            buf.append(line.rstrip("\\"))
        elif buf:
            buf.append(line)
            yield "".join(buf)
            buf = []
        else:
            yield line
    if buf:
        yield "".join(buf)


def scan(o: t.Iterator[str]) -> PushBackIterator:
    return PushBackIterator(_scan(iter(o)))


# type = comment | line | task


def parse(s: t.Iterator[str]) -> t.List[Node]:
    task_rx = re.compile(r"^(?P<task>\S+):\s*(?P<deps>.*)")

    def in_comment():
        r = []
        for line in s:
            if line.startswith("#"):
                r.append(line)
            elif not line.strip():
                r.append(line)
            else:
                s.pushback(line)
                break
        return {"type": "comment", "content": r}

    def in_task(*, task, deps):
        r = []
        for line in s:
            # todo: TASK: xxx (taget specific variable)
            if line.startswith("	"):
                r.append(line)
            else:
                s.pushback(line)
                break

        return {
            "type": "task",
            "name": task,
            "args": [x.strip() for x in shlex.split(deps)],
            "content": r,
        }

    def in_file():
        r = []
        for line in s:
            if line.startswith("#"):
                s.pushback(line)
                r.append(in_comment())
                continue

            m = task_rx.search(line)
            if m is not None:
                r.append(in_task(**m.groupdict()))
                continue

            r.append({"type": "line", "content": line})
        return r

    return in_file()


# untypedだとツライなー


def emit(parsed: t.List[Node]) -> None:
    for d in parsed:
        typ = d["type"]
        if typ == "line":
            print(d["content"])
        elif typ == "comment":
            for cline in d["content"]:
                print(cline)
        elif typ == "task":
            if not d["args"]:
                print(f"{d['name']}:")
            elif len(d["args"]) == 1 and d["args"][0] == ";":
                print(f"{d['name']}: ;")
            else:
                print(f"{d['name']}:\\")
                for arg in d["args"][:-1]:
                    print(f" {arg}\\")
                print(f" {d['args'][-1]}")
            for cline in d["content"]:
                print(cline)
        else:
            raise Exception(f"unexpected node: {d!r}")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="*", type=argparse.FileType("r"))
    args = parser.parse_args()

    files = args.files
    if len(files) == 0:
        files = [sys.stdin]
    for f in files:
        emit(parse(scan(f)))


if __name__ == "__main__":
    main()
