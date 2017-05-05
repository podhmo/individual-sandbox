# todo:
# - struct definition
# - method definition
# - writer

import goaway
r = goaway.get_repository()
f = r.package("main").file("hello.go")
fmt = f.import_("fmt")

with f.func("hello") as hello:

    @hello.body
    def body(m):
        m.stmt(fmt.Println("hello world"))


with f.func("add").args(r.int("x"), r.int("y")).returns(r.int) as add:

    @add.body
    def _(m):
        m.return_("{} + {}".format(add.x, add.y))


with f.func("main") as main:

    @main.body
    def body(m):
        m.stmt(hello())
        m.stmt(fmt.Println(1, "+", 2, add(1, 2)))


m = r.writer.write_file(f)
print(m)
