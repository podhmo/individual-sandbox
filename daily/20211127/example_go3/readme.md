# go entっぽい感じの設定方法を調べてみる

entのここの部分の実装 https://entgo.io/docs/schema-fields/

```go
// User schema.
type User struct {
    ent.Schema
}

// Fields of the user.
func (User) Fields() []ent.Field {
    return []ent.Field{
        field.Int("age"),
        field.String("name"),
        field.String("username").
            Unique(),
        field.Time("created_at").
            Default(time.Now),
    }
}
```

stringやtimeの部分

- https://github.com/ent/ent/blob/6372263b990c51eb21576f79a96faa362654f0e2/schema/field/field.go#L20-L26

### field

ここで `ent.Field` は以下のような型

```go
type Field interface {
    Descriptor() *field.Descriptor
}
```

field.Descriptorの定義

https://github.com/ent/ent/blob/6372263b990c51eb21576f79a96faa362654f0e2/schema/edge/edge.go#L13-L26

### builder

実際の`field.String()`などではbuilderを返している。

https://github.com/ent/ent/blob/6372263b990c51eb21576f79a96faa362654f0e2/schema/field/field.go#L20-L26

```go
// String returns a new Field with type string.
func String(name string) *stringBuilder {
	return &stringBuilder{&Descriptor{
		Name: name,
		Info: &TypeInfo{Type: TypeString},
	}}
}

// stringBuilder is the builder for string fields.
type stringBuilder struct {
	desc *Descriptor
}
```

あとは素直にメソッドを増やしまくっているだけ。

https://github.com/ent/ent/blob/6372263b990c51eb21576f79a96faa362654f0e2/schema/field/field.go#L168

```go
// Unique makes the field unique within all vertices of this type.
func (b *stringBuilder) Unique() *stringBuilder {
	b.desc.Unique = true
	return b
}
```

### types

便利な感じに独自定義しているのだけれど、実装が結構かっこいい

https://github.com/ent/ent/blob/6372263b990c51eb21576f79a96faa362654f0e2/schema/field/type.go