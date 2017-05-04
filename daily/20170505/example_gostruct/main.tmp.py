import gostruct
from prestring.go import Module

r = gostruct.get_repository(gostruct.VerboseStringer())
# r = gostruct.get_repository()
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
print(r.int("n"), r.int("n").as_argument(foo_file))
print(status("s"), status("s").as_argument(foo_file))
# foo_file.func("hello", args=foo_file.value)
bar_file = r.package("github.com/bar").file("bar.go")
status2 = bar_file.enum("Status2", r.string)
with status2 as member:
    member("x", 1)
    member("y", 2)
    member("z", 3)
print(status("s0").as_argument(foo_file))
print(status2("s1").as_argument(foo_file))
print(r.int("n").pointer)
print(r.int("n").pointer.pointer)
print(r.int("n").pointer.pointer.slice)
print(r.int("n").slice)
print(r.int("n").slice.pointer)
print(status2("s2").pointer.as_argument(foo_file))
