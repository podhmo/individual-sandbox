# todo:
# - struct definition
# - function definition (method definition)
# - writer

import gostruct
from prestring.go import GoModule
r = gostruct.get_repository()
f = r.package("main").file("hello.go")


hello = f.func("hello")
@hello.body
def hello(m):
    # m.import_("fmt")  # xxx
    m.stmt('fmt.Println("hello world")')

m = GoModule()
m.stmt(f.package)
print(m)
