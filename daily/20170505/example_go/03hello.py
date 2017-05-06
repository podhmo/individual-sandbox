from goaway import get_repository

r = get_repository()
f = r.package("main").file("main.go")


@f.func("main")
def body(m):
    fmt = m.import_("fmt")
    m.stmt(fmt.Println("Hello, 世界"))

print(r.writer.write(f))
