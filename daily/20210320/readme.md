## go 調べたいこと

- type aliasはreflectでunderline typeと区別できる？ -> type aliasはreflectで区別できない
- reflect中でsliceを見る

## go web api

- usecaseこそ必要なもの。それ以外は要らない。
- additionalPropertiesに対応するためにはunmarshalJSONを捨てた方が早い
- fastAPI的なDIをgoでシミュレート

### binder

### di

- map,slice,struct -> json body
- pointer (struct) -> component
- interface -> component
- function -> component
- pointer (primitive) -> query string
- primitive -> json body or path

生成されるもの

- registry (interface)
- bindFunction

```go
code = BindAction(
  "/users/{userId:int}",
  UpdateUser,
  WithAnnotation(Header("xxx"))
)
```

```go
type Code struct {
	Emit(ctx context, w io.Writer, filename string) error
    Symbol *Symbol
}

type Symbol struct {
	Name string
	Package *types.Package
}
```
