from prestring.python import Module

m = Module()

with m.def_("main"):
    m.stmt("print('hello world')")

with m.if_("__name__ == '__main__'"):
    m.stmt("main()")

print(m)
