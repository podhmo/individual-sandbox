from prestring.python import Module

m = Module()
with m.def_("hello"):
    m.stmt("""print("hello")""")
print(m)
