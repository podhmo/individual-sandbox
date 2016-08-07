# interfaceによるreceiverの選択

基本的にinterfaceでのsignature的なものは実行時の変換が行われずdispatchされる。
なので `MarshalJSON` をpointerをreceiverにして実装した場合にはpointerを渡してあげないとダメ。

```go
type A struct {
	Name string
}

func (a *A) MarshalJSON() ([]byte, error) {
	return []byte(`"a"`), nil
}

{
    target := A{Name: "foo"}
    {
        output, _ := json.Marshal(&target)
        fmt.Printf("a: %p, %s\n", &target, output)  // "a: 0xc82000a3b0, \"a\""
    }
    {
        output, _ := json.Marshal(target)
        fmt.Printf("a: %p, %s\n", &target, output)  // "a: 0xc82000a3b0, {\"Name\":\"foo\"}"
    }
}
```
