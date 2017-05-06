from prestring.go import GoModule

m = GoModule()
with m.import_group() as im:
    im("fmt")

with m.func("main"):
    m.stmt('fmt.Println("Hello, 世界")')
print(m)
