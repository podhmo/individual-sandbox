from prestring.go.codeobject import Module
from prestring import codeobject
import json

# print("**switch")
# codeobject.default_as_value = json.dumps

m = Module()
fmt = m.import_("fmt")

m.stmt("fmt.Println({})", "foo")
m.stmt("fmt.Println({!r})", "foo")

m.stmt(fmt.Println("foo"))
m.stmt("foo")
print(m)
