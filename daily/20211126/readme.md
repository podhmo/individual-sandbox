# memo

## go reflect.Value to literal

これって実はむずいんだろうか？

## go validator

- いい感じにエラーメッセージを返す方法
- i18n対応など考えたい
- formatなども考えたい

結果をJSONで返せば良いのでは？

なにをどう変換したいか？

- フィールド名
- グルーピング（validation名）
- エラーメッセージ

### customize validation

validatorのFieldErrorの意味

- tag and actual-tag ( `iscollor` か `hexcolor|rgb|rgba|hs|hsla` か)
- namespace and struct-namespace ( `Config.name` か `Config.Name` か)
- field and struct-field ( `name` か `Name` か)
- value and param (実際の値 か stringか？)
- kind (reflect.Kind)
- type (reflect.Type)
- translate (translator -> 変換後の文字列)
- Error (エラー)

```go
type Config struct {
    Name `json:"name" validate:"required"`
    Color `json:"color" validate:"iscolor"`
}
```

### メッセージの例

toJSON

```json
{
    "tag": "oneof",
    "translation": "{0}は[{1}]のうちいずれかでなければなりません",
    "args": ["X", ["S", "A", "B", "C", "D", "E"]],
    "field": "grade",
    "path": "Config#Grade"
}
```

toText (ltsv)

```
tag:oneof   path:   Config#Grade    message:Xは[S A B C D E]のうちいずれかでなければなりません  Args: {"X", {"S", "A", "B", "C", "D", "E"}}
```

toSimpleText

```
Config#Grade    oneof   message:Xは[S A B C D E]のうちいずれかでなければなりません
```

### 翻訳のとき

見ても全然わかんないな。。

```go
package ut // import "github.com/go-playground/universal-translator"

type Translator interface {
	locales.Translator

    Add(key interface{}, text string, override bool) error
	AddCardinal(key interface{}, text string, rule locales.PluralRule, override bool) error
	AddOrdinal(key interface{}, text string, rule locales.PluralRule, override bool) error
	AddRange(key interface{}, text string, rule locales.PluralRule, override bool) error
	T(key interface{}, params ...string) (string, error)
	C(key interface{}, num float64, digits uint64, param string) (string, error)
	O(key interface{}, num float64, digits uint64, param string) (string, error)
	R(key interface{}, num1 float64, digits1 uint64, num2 float64, digits2 uint64, param1, param2 string) (string, error)
	VerifyTranslations() error
}
```