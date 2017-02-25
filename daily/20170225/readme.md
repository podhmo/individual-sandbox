# swaggerとjsonschemaの事まとめる

- required=Trueは付けておいたほうが良い(defaultはoptional)
- ちょくちょくjsonschemaと違う部分がある？
- 即時定義は展開した形で渡したほうが良い
- $refを多用するとその場その場で適切な注釈を付けられなくなるので注意(allOfを使った迂回作がある)
- allOfの解釈が怪しい
- swagger-uiもあるけれどredocの方が良さ気

## requiredのこと

required=Trueを付けていた方が良いと言うのは一応jsonschemaの復習だけれど。

このschemaは

```yaml
type: object
  properties:
    name:
      type: string
```

以下全部valid

```js
{}
{"nnname": "foo"}
{"name": "foo"}
```

requiredを付けましょう。

## 即時定義のこと

```yaml
definitions:
  A:
    type: object
    properties:
      b:
        type: object
        properties:
          name:
            type: string
```

は、こうしましょうということ(ただし自分で気をつけるのはナンセンスなので何らかのツールで変換するべき)。

```yaml
definitions:
  A:
    type: object
    properties:
      b:
        $ref: "#/definitions/AB"
  AB:
    type: object
    properties:
      name:
        type: string
```

たとえば、ある時期標準のswagger codegenが以下のような位置にある `x-something` が読めなくなっていた。

```yaml
definitions:
  A:
    type: object
    properties:
      b:
        x-something: xxxx
        type: object
        properties:
          name:
            type: string
```


## allOfのこと

これは無理(jsonschemaの仕様)。$refの存在する箇所では$refしか持てない(参照という意味では妥当そう)。

```yaml
$ref: "#/definitions/foo"
description: this is description
```

allOfを使った迂回策というのはこういうこと

```yaml
allOf:
 - $ref: "#/definitions/foo"
description:
 hello this is current level description
```

ただ可能ならこちらのほうが良い(個人的に)。しかしサポートされていない場合もある。


```
allOf:
 - $ref: "#/definitions/foo"
 - description: hello this is current level description
```


allOfの解釈が怪しいと言うのは

```yaml
allOf:
  - $ref: "#/definitions/foo"
  - required:
    - x
    - y
```

がダメで。代わりに以下の様に書かないとダメなところ。どうもmixした結果をvalidationするのではなく、個別に１個１個validationしている(仕様ではなくtoolの問題)。

```yaml
allOf:
  - $ref: "#/definitions/foo"
required:
  - x
  - y
```

ちなみにこれが大丈夫なのは、swagger editorやswagger公式の方だけで。go-swaggerはそもそも対応していない。

# go-swaggerのことをまとめる

- requestは自動でvalidate
- responseは自動でvalidateしてくれない
- format定義されている系を$refで取るとvalidation失敗
- requestはnullと渡さずを関知しない(実害はない)
- responseはomitemptyでtype=nullがない部分でnullが変えることはない。(変に迂回して保存されない限り。slicesについても大丈夫なはず)
- schemaのtypeに配列が渡された場合には先頭だけを関知する。( `type:["string","null"]` ではなくx-nullable:trueを変わりに使う)
- configure_<appname>.goでhandler設定する。(ただし次回移行新しいものが生成されないのでなんか使いづらい)
- go-swaggerではまともにallOfが動かないと思ったほうが良い。

## responseは自動でvalidateしてくれない

repsonseは自動でvalidateしてくれないので注意。validate忘れると辛い感じになる(ゼロ値がそのまま変える)。

## omitemptyの話

型との整合性はほとんどomitemptyに依存している。

## 色々な指定

以下のような結果になる感じ。

```yaml
    properties:
      name:
        type: string
      name2:
        x-nullable: true
        type: string
      name3:
        type: string
      name4:
        x-nullable: true
        type: string
      name5:
        type:
          - string
          - null
      name6:
        type:
          - string
          - null
    required:
      - name3
      - name4
      - name6
```

```go
	// name
	Name string `json:"name,omitempty"`

	// name2
	Name2 *string `json:"name2,omitempty"`

	// name3
	// Required: true
	Name3 *string `json:"name3"`

	// name4
	// Required: true
	Name4 *string `json:"name4"`

	// name5
	Name5 string `json:"name5,omitempty"`

	// name6
	// Required: true
	Name6 *string `json:"name6"`
```


## serverを生成せずに色々調べたい。

```
$ swagger init spec  # swaggerの雛形の生成
$ swagger generate model -f swagger.yml -t .  # modelsだけ生成
```

modelのvalidation

```go
// import 	"github.com/go-openapi/strfmt"

registry := strfmt.NewFormats(
s := models.<Schema>{}
err := s.Validate(registry)
```

modelのdeserialize

```go
// import	"github.com/go-openapi/swag"

swag.ReadJSON(b, &ob)
```

modelのserialize

```go
// import	"github.com/go-openapi/swag"

swag.WriteJSON(&ob)
```

## validationのこと

requestのvalidation `Params.BindRequest()` で行われる。
responseのvalidation は **行われない** 。こういうの書くのが無難。

```go
registry := strfmt.NewFormats()
if err := payload.Validate(registry); err != nil {
	return &errorResponse{status: 500, err: err}
}

type errorResponse struct {
	status int
	err    error
}

// WriteResponse to the client
func (o *errorResponse) WriteResponse(rw http.ResponseWriter, producer runtime.Producer) {
	rw.WriteHeader(o.status)
	fmt.Fprintf(rw, "%q", o.err)
}
```

## bug

- responseの中のAllOfを上手く認識しない？
- $refのformatを認識しない

[bugの例](https://gist.github.com/podhmo/9126fbb4de7119e9ab5ff7d90aaaed79)
[bugの例2](https://gist.github.com/podhmo/4df73990ac9f3794a8cc396bec2c948c)
