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
