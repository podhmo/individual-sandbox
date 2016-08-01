# golangのembedなどで名前が衝突した時の動作

- embedしたものとの名前の衝突(親と子)
- embedしたものとの名前の衝突衝突(子1と子2)
- embedしたのと同名のtype alias

## embedしたものとの名前の衝突(親と子)

親が優先されるはず。

```go
type Person struct {
	Id   string
	Name string
	Age  int
}

type WrappedPerson struct {
	Id int
	Person
}
```

期待通り。

```
person = main.Person{Id:"p1", Name:"Foo", Age:20}
output: {"Id":"p1","Name":"Foo","Age":20}
----------------------------------------
wrapped person = main.WrappedPerson{Id:1, Person:main.Person{Id:"p1", Name:"Foo", Age:20}}
output: {"Id":1,"Name":"Foo","Age":20}
```

## embedしたものとの名前の衝突衝突(子1と子2)

予想: compile errorになるか。後のものが優先される。

```go
type Child1 struct {
	Id   string
	Name string
}

type Child2 struct {
	Id    int
	Value int
}

type Person struct {
	Child1
	Child2
}
```

予想外。衝突した部分(上の例ではId)は無視される模様

```
person: main.Person{Child1:main.Child1{Id:"p1", Name:"foo"}, Child2:main.Child2{Id:2, Value:10}}
output: {"Name":"foo","Value":10}
```

## embedしたのと同名のtype alias

cast出来るか調べてみる。そもそも以下はcastできない。

```go
type MyInt int
type Person struct {
	Name string
	Age  int
}

type MyPerson struct {
	Name string
	Age  MyInt
}
type MyPerson2 struct {
    Person
	Age  MyInt
}
```

また以下の様に書いてしまっていた場合には、値を渡さないとzero値で初期化されてしまう。

```go
type MyPerson struct {
    *Person
	Age  MyInt
}

mp := MyPerson{Person: &Person{Name: "foo", Age: 20}}
output, _ := json.Marshal(&mp)
// => {"Name":"foo","Age":0}
```

こういう感じの変換を書いておくのが無難。あるコンテキスト上では全部ForMarshalみたいなもので変換してからjson.Marshalを実行すると良いかもしれない。

```go
func (p Person) ForMarshal() MyPerson {
	return MyPerson{Person: &p, Age: MyInt(p.Age)}
}

p := Person{Name: "foo", Age: 20}.ForMarshal()
mp := p.ForMarshal()
output, _ := json.Marshal(&mp)
// => {"Name":"foo","Age":20}
```

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
