output

```console
$ go run main.go
// このような形で内部では使われる
[{foo0} {foo1} {foo2} {foo3} {foo4} {foo5}] <nil>

// これをAPIとして公開したいときにhttp.Handlerとして扱う
API response: 200 OK
[{"Name":"foo0"},{"Name":"foo1"},{"Name":"foo2"},{"Name":"foo3"},{"Name":"foo4"},{"Name":"foo5"}]

// しかし、paginationなどを考えるとresponseをwrapしたい
API response: 200 OK
{"items":[{"Name":"foo0"},{"Name":"foo1"},{"Name":"foo2"},{"Name":"foo3"},{"Name":"foo4"},{"Name":"foo5"}],"info":{"hasNext":false}}

// 個別にhandlerを書くのが面倒
API response: 200 OK
{"items":[{"Name":"foo0"},{"Name":"foo1"},{"Name":"foo2"},{"Name":"foo3"},{"Name":"foo4"},{"Name":"foo5"}],"info":{"hasNext":false}}

// paginationが動いて欲しい
API response: 200 OK /?page=0
{"items":[{"Name":"foo0"},{"Name":"foo1"},{"Name":"foo2"}],"info":{"hasNext":true,"nextId":"1"}}
API response: 200 OK /?page=1
{"items":[{"Name":"foo3"},{"Name":"foo4"},{"Name":"foo5"}],"info":{"hasNext":false}}
API response: 200 OK /?page=2
{"items":[],"info":{"hasNext":false}}
```
