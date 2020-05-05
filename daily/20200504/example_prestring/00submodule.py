from prestring import Module

m = Module()
m.stmt("// start")
sm = m.submodule("")
m.stmt("// end")

with sm.scope():
    sm.stmt("- foo")
    sm.stmt("- bar")
    sm.stmt("- boo")

print(m)
