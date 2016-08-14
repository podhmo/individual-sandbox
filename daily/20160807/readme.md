# pointerを渡さなければもちろん毎回copyされる

type aliasであってもstrucctであっても。

```go
type Foo struct {
	name string
}

func foo(foo Foo) {
	fmt.Printf("%p %s\n", &foo, foo.name) // copied
}
func foo2(foo *Foo) {
	fmt.Printf("%p %s\n", foo, foo.name)
}
```

# interfaceによるreceiverの選択

基本的にinterfaceでのsignature的なものは実行時の変換(`a.Foo` が `(&a).Foo` とみなされるようなやつ)が行われずdispatchされる。

```go
type Greeter interface {
	Greet() string
}

type Foo struct {
	name string
}

func (f *Foo) Greet() string {
	return fmt.Sprintf("pointer of %s", f.name)
}

func foo(f Greeter) string {
	return f.Greet()
}

f := Foo{name: "foo"}
// fmt.Println(foo(f)) // compile error
fmt.Println(foo(&f)) // => "pointer of foo"
```

こういうエラーメッセージ。

```
use f (type Foo) as type Greeter in argument to foo:
	Foo does not implement Greeter (Greet method has pointer receiver)
```

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
