# golang 簡単なhttptestのsample

- [example_server](./example_server)

もう少ししたいこと

- GET以外のrequest
- 複数のrequestのchain

更にしたいこと

- http以外のprotocol話すやつのmock

# golang JSONのencoder/decoder使っていきたい

- https://golang.org/pkg/encoding/json/#example_Decoder
- https://golang.org/pkg/encoding/json/#Decoder

# golang JSONのpretty print

marshalする時に[json.MarshalIndent](https://golang.org/pkg/encoding/json/#MarshalIndent) の方を使えば良い。

```go
person := &Person{Name: "foo", Age: 20}
b, err := json.MarshalIndent(person, "", "  ")
```

# golang quoteされたstringのunquoteの方法

[strconv.Unquote](https://golang.org/pkg/strconv/#Unquote) を使うと良い。

```go
gore> :import fmt
gore> :import strings
gore> x := fmt.Sprintf("%q", "foo")
(string)"foo"
gore> :import strconv
gore> strconv.Unquote(x)
"foo"
(string)foo
(interface {})<nil>
```
