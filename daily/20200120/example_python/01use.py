import foo
import foo.bar
import foo.boo

print(getattr(foo, "__file__", None))
