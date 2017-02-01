# python objectへのpatchどうだったっけ？

mock.patch.object とかできた記憶。

```python
class Foo(object):
    def foo(self):
        return "foo"

foo = Foo()
print(foo.foo())  # "foo"

with mock.patch.object(foo, "foo") as m:
    m.side_effect = Exception("oops")
    try:
        print(foo.foo())
    except Exception as e:
        print(e)  # oops

print(foo.foo())  # foo
```

# golang interfaceを埋め込んでもJSON化した時にflatにならない。

以下みたいなやつ

```
type IPereson interface {}

type PersonWithInterface struct {
	IPerson
}
```

# jsonschema json reference pathのescapeで参考にすること

json pointerの方の例にかいてあったっぽい。
https://tools.ietf.org/html/rfc6901#section-3


