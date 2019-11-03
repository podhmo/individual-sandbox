from prestring import INDENT, UNINDENT


def _transform(source: str, *, m, indent):
    history = [0]
    for line in source.splitlines():
        if line == "":
            m.stmt("m.sep()")
            continue
        lv = 0
        while line.startswith(indent):
            lv += 1
            line = line[len(indent) :]

        prev_lv = history[-1]
        history.append(lv)
        while prev_lv < lv:
            m.stmt("with m.scope():")
            m.append(INDENT)
            lv -= 1
        while prev_lv > lv:
            m.append(UNINDENT)
            lv += 1
        m.stmt("m.stmt({!r})", line)
    return m


def transform(source: str, *, indent="\t"):
    from prestring.text import Module

    m = Module(indent=indent)
    m.stmt("from {} import {}", m.__class__.__module__, m.__class__.__name__)
    m.stmt("m = Module(indent={!r})", indent)
    m = _transform(source, m=m, indent=indent)
    m.stmt("print(m)")
    return m


if __name__ == "__main__":
    import pathlib

    with open(pathlib.Path(__file__).parent / "03text/main.go.template") as rf:
        text = rf.read()
    print(transform(text))
