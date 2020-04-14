import sys
import re
import textwrap
from prestring.output import output
from prestring.text import Module

rx = re.compile("..\s+pygal-code::")
is_reading = False

r = []
buf = []
lines = iter(sys.stdin)
for line in lines:
    line = line.rstrip("\n")
    if not is_reading:
        m = rx.search(line)
        if m is None:
            continue
        is_reading = True
        next(lines)
        continue

    if not line.startswith(" "):
        is_reading = False
        r.append(buf)
        buf = []
        continue
    buf.append(line)


rx2 = re.compile(r"^\s+([a-zA-Z0-9_]+)\.")
with output("output") as fs:
    with fs.open("Makefile", "w", opener=Module) as mk:
        mk.stmt("SHELL := $(shell which bash)")
        mk.append("default: ")
        mdefault_deps = mk.submodule()
        mk.stmt("")

        for i, buf in enumerate(r):
            code = "\n".join(buf)
            if "..." in code:
                continue

            mdefault_deps.append(f"{i:03d} ")

            mk.stmt(f"{i:03d}:")
            mk.stmt(
                f"	python $(shell echo code$@.py) |& tee $(patsubst %.py,%.svg,$(shell echo code$@.py))"
            )

            with fs.open(f"code{i:03d}.py", "w") as wf:
                name = None
                for rline in reversed(buf):
                    m = rx2.search(rline)
                    if m is not None:
                        name = m.group(1)
                        break
                if name is None:
                    break

                print("import pygal", file=wf)
                print(textwrap.dedent(code), file=wf)
                print(f"print({name}.render(is_unicode=True))", file=wf)
