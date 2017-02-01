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


