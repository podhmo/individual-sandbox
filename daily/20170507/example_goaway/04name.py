from goaway import get_repository


def define(f):
    person = f.struct("Person")
    person.define_field("Name", f.string)

    p = person("p")

    @f.method("getName", p, returns=f.string)
    def getName(m):
        m.return_(p.Name)


r = get_repository()
f = r.package("main").file("main.go")
define(f)
print(r.writer.write(f))
