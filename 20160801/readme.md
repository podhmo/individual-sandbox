# golangでjsonのoutput

- marshal時にsnake_caseにする
- 不要なフィールドを取り除く
- 異なるmarshalの方法を与える
- 付加的な情報を与える

元のstruct

```go
type Person struct {
	FirstName string
	LastName string
	CreatedAt time.Time
}
```

## marshal時にsnake_caseにする

tag付ける

```go
type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt time.Time `json:"created_at"`
}
```

## 不要なフィールドを取り除く

"-" のtagを付ける

```go
type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt time.Time `json:"-"`
}
```

## 異なるmarshalの方法を与える


`MarshalJSON()` を書く

```go
type MyTime time.Time

type Person struct {
	FirstName string `json:"first_name"`
	LastName  string `json:"last_name"`
	CreatedAt MyTime `json:"created_at"`
}

func (m *MyTime) MarshalJSON() ([]byte, error) {
	return []byte(`"` + m.format() + `"`), nil
}

func (m *MyTime) format() string {
	return (time.Time(*m)).Format("2006-01-02 15:04:05")
}
```

## 付加的な情報を与える

`MarshalJSON()` 内で元の型を埋め込んだ型を使う

```go
type WithTimestampPerson Person

func (p WithTimestampPerson) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
        LastSeen int64 `json: "last_seen"`
        Person
	}{
        Person: Person(p), LastSeen: time.Now().Unix(),
	})
}

func main(){
	{
		person := Person{FirstName: "foo", LastName: "bar", CreatedAt: time.Now()}
		output, _ := json.Marshal(WithTimestampPerson(person))
		fmt.Printf("output: %s\n", output)
	}
}
```

## おまけ MarshalJSONはpointerにしないほうが良いかも

以下の様に書いている時、pointerを渡さないと期待した出力にならない。

```go
func (p *MyPerson2) MarshalJSON() ([]byte, error) {
	return json.Marshal(&struct {
		FullName string `json:"full_name"`
	}{
		FullName: p.FirstName + " " + p.LastName,
	})
}
```
