from prestring import Module


def emit_internal(m: Module) -> None:
    with m.scope():
        m.stmt("- foo")
        m.stmt("- bar")
        m.stmt("- boo")


m = Module()
m.stmt("// start")
with m.scope():
    m.stmt("itemize")
    emit_internal(m)
m.stmt("// end")

print(m)
