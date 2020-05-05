from prestring import Module

sm = Module()
with sm.scope():
    sm.stmt("- foo")
    sm.stmt("- bar")
    sm.stmt("- boo")

m = Module()
m.stmt("// start")
with m.scope():
    m.stmt("itemize")
    m.submodule(sm, newline=False)
m.stmt("// end")
print(m)
