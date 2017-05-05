import goaway
from prestring.go import Module

r = goaway.get_repository(goaway.VerboseStringer())
r = goaway.get_repository()
models = r.package("github.com/foo/models")
foo_file = models.file("foo.go")
database = foo_file.import_("github.com/foo/database")

status = foo_file.enum("Status", r.string)
with status as member:
    member("ok", "OK")
    member("ng", "NG")

print(status.members)
print(models)
print(foo_file)
print(database)
print(status)
print(r.int("n"), r.int("n").withtype(foo_file))
print(status("s"), status("s").withtype(foo_file))
# foo_file.func("hello", args=foo_file.value)
bar_file = r.package("github.com/bar").file("bar.go")
status2 = bar_file.enum("Status2", r.string)
with status2 as member:
    member("x", 1)
    member("y", 2)
    member("z", 3)
print(status("s0").withtype(foo_file))
print(status2("s1").withtype(foo_file))
print(r.int("n").pointer)
print(r.int("n").pointer.pointer)
print(r.int("n").pointer.pointer.slice)
print(r.int("n").slice)
print(r.int("n").slice.pointer)
print(status2("s2").pointer.withtype(foo_file))

print(foo_file.func("hello").args(r.int("x"), r.int("y")).returns(r.int).typename(foo_file))
print(foo_file.func("hello").args(r.int("x"), r.int("y")).typename(foo_file))
print(foo_file.func("hello").typename(foo_file))

hello = foo_file.func("hello").args(r.int("x"), r.int("y")).returns(r.int)
@hello.body
def hello_body(m):
    m.stmt("fmt.Println('hai')")

add = foo_file.func("add").args(r.int("x"), r.int("y")).returns(r.int)
@add.body
def add(m):
    print("hoi")
    m.return_("{} + {}".format(add.x, add.y))

m = Module()
add(m)
print("-")
print(m)


fmt = r.package("fmt")
print(fmt.Println("hello %d %d", add.x, 10))
