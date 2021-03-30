## go hclを自分で使ってみる

terraformの設定ファイルを読み込んでいるのって何があったっけ？
terraform fmtあたりを参考にする？いや、confの中身を覗いている物が良い
validateかoutputかplan。まぁ素直に考えるならplan。

そもそもどこでsubcommandを実装しているかわからないな。これを使っているのでCommandsを探そう。

- https://github.com/mitchellh/cli

### 知りたいこと

知りたいことってなんだろう？

- decode
- encode
- format
- 自作の関数の埋め込み方
- 定義の仕方

### planの実装

```
2021/3/26 23:36 [$HOME/ghq/github.com/hashicorp/terraform/configs/module.go]
	"github.com/hashicorp/hcl/v2" とりあえずhcl/v2を使えば良いらしい。

2021/3/26 23:36 [$HOME/ghq/github.com/hashicorp/terraform/configs/module.go]
Moduleとかも自分で作っているのか。。

2021/3/26 23:35 [$HOME/ghq/github.com/hashicorp/terraform/configs/configload/loader.go]
configs/configloadというパッケージがあるのでその付近でconfは読み込まれている。

2021/3/26 23:34 [$HOME/ghq/github.com/hashicorp/terraform/command/meta_config.go]
configLoaderをinitとしてparserを取り出してLoadConfigDirか

2021/3/26 23:34 [$HOME/ghq/github.com/hashicorp/terraform/command/meta_config.go]
それはloadSingleModule

2021/3/26 23:33 [$HOME/ghq/github.com/hashicorp/terraform/command/plan.go]
type PlanCommand struct {
	Meta
}
と言う感じでMetaを埋め込んで、ここにloadBackendConfig()が記録されている。

2021/3/26 23:33 [$HOME/ghq/github.com/hashicorp/terraform/command/plan.go]
loadBackendConfigを覗けば十分だったりする？もしかして？

2021/3/26 23:32 [$HOME/ghq/github.com/hashicorp/terraform/command/plan.go]
普通に素直にcommand/plan.goにあるのか。

2021/3/26 23:32 [$HOME/ghq/github.com/hashicorp/terraform/commands.go]
graphなんてコマンドもあるんだ

2021/3/26 23:31 [$HOME/ghq/github.com/hashicorp/terraform/commands.go]
なるほど。terraform/commands.goに書かれていた。

2021/3/26 23:31 [$HOME/ghq/github.com/hashicorp/terraform/commands.go]
TF_DATA_DIRなんてあるんだ

2021/3/26 23:30 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
initCommands

2021/3/26 23:26 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
discoって何者？

2021/3/26 23:26 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
mitchellh/cliを使っているのか。mitchellh/colorstringってどんなものなんだろ？

2021/3/26 23:24 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
cliRunnerのRun()を追えばよいはず。

2021/3/26 23:24 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
didyoumeanで頑張ってsuggestionを表示しているのそういえば偉いよなー。

2021/3/26 23:23 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
cliRunnerのsubcommandの-を_に変換しているのは何なんだろ？

2021/3/26 23:23 [$HOME/ghq/github.com/hashicorp/terraform/main.go]
cli.CLIでrunnerを作っている。

```

普通にこの辺を見れば良さそう

- https://pkg.go.dev/github.com/hashicorp/hcl/v2


### hclsimple

何かそれっぽいコードは発見したが、これの設定ファイルはどうなっているんだろう？

```go
type Config struct {
	LogLevel string `hcl:"log_level"`
}
```

なるほど。以下に対応する。

```
log_level = "info"
```

- https://stackoverflow.com/questions/66460117/parse-simple-terraform-file-using-go

### variable

それじゃ、variableみたいなやつは？

```go
type Config struct {
	Variable []Variable `hcl:"variable,block"`
}

type Variable struct {
	Value   string `hcl:"name,label"`
	Default string `hcl:"default"`
}

```

このように書くと以下の様に取れる。labelって結構特別な意味合いのものなんだろうか？

```
variable "projec-tname" {
  default = "xxx"
}

variable "user" {
  default = "foo"
}
```

なるほどね。使い方がsuffixにつくイメージ。blockとlabel。他は何があるんだろう？

あと気になるのは以下。

- formatをどうやってする？
- 文字列置換などはデフォルトで入っている？

## hclの方を覗く

どうやらhcl2 branchの方で開発が進んでいるらしい。

```
$ ghq get hashicorp/hcl
$ cd <>
$ git checkout hcl2
```

というか、まずはこういうことをしても良かった。

```console
$ go doc github.com/hashicorp/hcl/v2/hclsimple
package hclsimple // import "github.com/hashicorp/hcl/v2/hclsimple"

Package hclsimple is a higher-level entry point for loading HCL
configuration files directly into Go struct values in a single step.

This package is more opinionated than the rest of the HCL API. See the
documentation for function Decode for more information.

func Decode(filename string, src []byte, ctx *hcl.EvalContext, target interface{}) error
func DecodeFile(filename string, ctx *hcl.EvalContext, target interface{}) error
```

なるほど。ctxをどうするかと言う話っぽい。.jsonだったらjsonを取るし.hclだったらhclsyntax.ParseConfigを呼ぶらしい。
あ、でもこのjsonはencoding/jsonではなくhcl/json

```go
func Decode(filename string, src []byte, ctx *hcl.EvalContext, target interface{}) error {
	var file *hcl.File
	var diags hcl.Diagnostics

	switch suffix := strings.ToLower(filepath.Ext(filename)); suffix {
	case ".hcl":
		file, diags = hclsyntax.ParseConfig(src, filename, hcl.Pos{Line: 1, Column: 1})
	case ".json":
		file, diags = json.Parse(src, filename)
	default:
		diags = diags.Append(&hcl.Diagnostic{
			Severity: hcl.DiagError,
			Summary:  "Unsupported file format",
			Detail:   fmt.Sprintf("Cannot read from %s: unrecognized file format suffix %q.", filename, suffix),
		})
		return diags
	}
	if diags.HasErrors() {
		return diags
	}

	diags = gohcl.DecodeBody(file.Body, ctx, target)
	if diags.HasErrors() {
		return diags
	}
	return nil
}
```

実用上はこれ経由がいいけれど、依存を減らすなら `github.com/hashicorp/hcl/v2/hclsyntax.ParseConfig()` を呼ぶらしい。
jsonを取り出したあとにも色々ctxを加えられるっぽい？どちらかというと気になるのは、 `gohcl.DecodeBody()`のほうか。

知りたいのはEvalContextの部分か。

### EvalContext

別に特別なものでもないな。hclは結構内部的にはreflect的なもの。
そうそう知りたいのは `	"github.com/zclconf/go-cty/cty/function"` か。

```
2021/3/27 0:8 [$HOME/ghq/github.com/hashicorp/hcl/gohcl/schema.go]
あー、この辺じゃん attr,block,label,remain,body,optionalのタグがある

2021/3/27 0:8 [$HOME/ghq/github.com/hashicorp/hcl/gohcl/schema.go]
		switch kind {
		case "attr":
			ret.Attributes[name] = i
		case "block":
			ret.Blocks[name] = i
		case "label":
			ret.Labels = append(ret.Labels, labelField{
				FieldIndex: i,
				Name:       name,
			})
		case "remain":
			if ret.Remain != nil {
				panic("only one 'remain' tag is permitted")
			}
			idx := i // copy, because this loop will continue assigning to i
			ret.Remain = &idx
		case "body":
			if ret.Body != nil {
				panic("only one 'body' tag is permitted")
			}
			idx := i // copy, because this loop will continue assigning to i
			ret.Body = &idx
		case "optional":
			ret.Attributes[name] = i
			ret.Optional[name] = true
		default:
			panic(fmt.Sprintf("invalid hcl field tag kind %q on %s %q", kind, field.Type.String(), field.Name))
		}
	}


2021/3/27 0:7 [$HOME/ghq/github.com/hashicorp/hcl/gohcl/encode.go]
encodeもあるのか一応。

2021/3/27 0:5 [$HOME/ghq/github.com/hashicorp/hcl/eval_context.go]

// An EvalContext provides the variables and functions that should be used
// to evaluate an expression.
type EvalContext struct {
	Variables map[string]cty.Value
	Functions map[string]function.Function
	parent    *EvalContext
}

// NewChild returns a new EvalContext that is a child of the receiver.
func (ctx *EvalContext) NewChild() *EvalContext {
	return &EvalContext{parent: ctx}
}

// Parent returns the parent of the receiver, or nil if the receiver has
// no parent.
func (ctx *EvalContext) Parent() *EvalContext {
	return ctx.parent
}
```

### 関数の定義場所が知りたい

組み込みの関数がどこにあるか？とか自作の関数を組み込むにはどうするかとか。

```
2021/3/27 7:15 [/Users/nao/ghq/github.com/zclconf/go-cty/cty/function/stdlib/bool.go]
あー、この辺にあったのか

2021/3/27 6:49 [$HOME/ghq/github.com/hashicorp/hcl/ext/userfunc/README.md]
と思ったらここは文法の拡張の話なのでやりすぎ

2021/3/27 6:42 [$HOME/ghq/github.com/hashicorp/hcl/ext/README.md]
これ見れば良いんじゃん？
```

- https://github.com/hashicorp/hcl/blob/v2.9.1/guide/go_expression_eval.rst

```go
package main

import (
	"github.com/zclconf/go-cty/cty/function"
	"github.com/zclconf/go-cty/cty/function/stdlib"
)

var specFuncs = map[string]function.Function{
	"abs":        stdlib.AbsoluteFunc,
	"coalesce":   stdlib.CoalesceFunc,
	"concat":     stdlib.ConcatFunc,
	"hasindex":   stdlib.HasIndexFunc,
	"int":        stdlib.IntFunc,
	"jsondecode": stdlib.JSONDecodeFunc,
	"jsonencode": stdlib.JSONEncodeFunc,
	"length":     stdlib.LengthFunc,
	"lower":      stdlib.LowerFunc,
	"max":        stdlib.MaxFunc,
	"min":        stdlib.MinFunc,
	"reverse":    stdlib.ReverseFunc,
	"strlen":     stdlib.StrlenFunc,
	"substr":     stdlib.SubstrFunc,
	"upper":      stdlib.UpperFunc,
}

var specCtx = &hcl.EvalContext{
	Functions: specFuncs,
}

```

### fmt 

hclfmtがあるじゃん

```
go get -v github.com/hashicorp/hcl/cmd/...
```

### syntax

- https://github.com/hashicorp/hcl/blob/v2.9.1/hclsyntax/spec.md
