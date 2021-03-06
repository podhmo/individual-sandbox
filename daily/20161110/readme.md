# golang 30分後など取得する

```go
now := time.Now()
now.Add(30 * time.Minute)
```

# golang 日時をてきとうに文字列から作る

```go
t, err := time.Parse(time.RFC3339, "2016-11-02T00:00:00Z")
// 2016-11-02 00:00:00 UTC
```

# golang nowをmockできるようにするやつ

これなんでこんなに大変なんだろ。。

```go
nowgen := NewMockNowGen()
ModifyWith(nowgen,
    func(m map[NowGenUseCase]time.Time) {
        fmt.Println("before mock", nowgen.Now())
        mockNow, _ := time.Parse(time.RFC3339, "2000-01-01T00:00:00Z")
        m[NowGenUseCaseNow] = mockNow
        fmt.Println("after mock", nowgen.Now())
    }, true)
fmt.Println("after clear", nowgen.Now())
```

# golang keyの範囲を制限したdict

構造体作れば良いだけですね。。

```go
/*
{
  "foo": "Foo",
  "bar": "Bar",
}
*/

type O struct {
	Foo string `json:"foo"`
	Bar string `json:"bar"`
}
```

# golang reflect iterate struct

https://golang.org/pkg/reflect/

```go
type Person struct {
	Name string
	Age  int
}

func main() {
	rt := reflect.ValueOf(&Person{}).Elem()
	for i := 0; i < rt.NumField(); i++ {
		v := rt.Field(i)
		t := rt.Type().Field(i)
		fmt.Printf("name: %v, value: %v\n", t.Name, v.Interface())
	}
}
```
