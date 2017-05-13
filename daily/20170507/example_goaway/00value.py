from goaway import get_repository

r = get_repository()
f = r.package("value").file("value.go")

value = f.struct("Value", comment="value type")
value.define_field("Name", type=f.string)
value.define_field("Value", type=f.int)

print(r.writer.write(f))
