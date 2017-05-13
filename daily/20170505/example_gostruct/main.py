import goaway

r = goaway.get_repository()
f = r.package("main").file("main.go")
fmt = f.import_("fmt")


@f.func("hello")
def hello(m):
    m.stmt(fmt.Println("hello world"))


@f.func("add", args=(r.int("x"), r.int("y")), returns=r.int)
def add(m):
    m.return_("{} + {}".format(add.x, add.y))


@f.func("main")
def main(m):
    m.stmt(hello())
    m.stmt(fmt.Printf("1 + 1 = %d\n", add(1, 2)))


if __name__ == "__main__":
    m = r.writer.write_file(f)
    print(m)
