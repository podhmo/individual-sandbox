from prestring.go import Module

m = Module()
m.package("main")

with m.import_group() as im:
    im.import_("fmt")

with m.func("main"):
    m.stmt("fmt.Println(`hello world`)")

print(m)
