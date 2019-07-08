import re
from handofcats import as_command


def distinct(
    lines,
    *,
    tb_line_rx=re.compile(
        r'^  File "(?P<filename>[^"]+)", line (?P<line>\d+), in (?P<where>.+)$'
    ),
):
    """
      File "00traceback.py", line 21, in <module>
        f()
    """

    itr = iter(lines)
    for line in itr:
        line = line.rstrip()
        m = tb_line_rx.search(line)
        if m is None:
            yield None, line
            continue

        try:
            next_line = next(itr)
            if not next_line.startswith("    "):
                yield None, line
                yield None, next_line
                continue

            fn_name = next_line.strip()
            if fn_name[0] != next_line[4]:
                yield None, line
                yield None, next_line
                continue

            row = {"code": fn_name, **m.groupdict()}
            row["line"] = int(row["line"])
            yield row, None
        except StopIteration:
            return


def parse(lines):
    return (d for d, other_line in distinct(lines) if d is not None)


@as_command
def run(filename: str = "./00.output") -> None:
    with open(filename) as rf:
        for d in parse(rf):
            print(d)
