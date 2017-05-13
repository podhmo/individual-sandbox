from goaway import get_repository

r = get_repository()
f = r.package("value").file("value.go")

with f.struct("Value", comment="value type") as field:
    field("Name", type=f.string)
    field("Value", type=f.int)

print(r.writer.write(f))
