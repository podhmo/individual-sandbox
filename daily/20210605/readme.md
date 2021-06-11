## apikit

- logger
- renderer

## go fiber

recipe

- graceful shutdown https://github.com/gofiber/recipes/tree/master/graceful-shutdown
- auth jwt https://github.com/gofiber/recipes/tree/master/auth-jwt

defaultで有効になってほしいmiddlewareってなんだろ？

- pprof
- timeout
- logger
- recoevr
- requestid
- (cors)

### testは?

app.Test()を使う。ここがhttp.Request,http.Responseを使うように調整されているみたい。

```go
// myfile_test.go
package main

import (
	"net/http/httptest"
	"testing"

	fiber "github.com/gofiber/fiber"
	utils "github.com/gofiber/utils"
)

// go test -run -v Test_Handler
func Test_Handler(t *testing.T) {
	app := New()

	app.Get("/test", func(c *Ctx) {
		c.SendStatus(400)
	})

	resp, err := app.Test(httptest.NewRequest("GET", "/test", nil))

	utils.AssertEqual(t, nil, err, "app.Test")
	utils.AssertEqual(t, 400, resp.StatusCode, "Status code")
}
```

## goのapi serverの実装に必要なものを整理

- portの変更
- graceful shutdown
- middleware
- error handling

2nd

- application error
- health check
- default logger
- openapi doc (path)


### graceful shutdown

- https://golang.org/pkg/net/http/#Server.Shutdown
- https://stackoverflow.com/questions/39320025/how-to-stop-http-listenandserve

## terraform applyの実行を追ってみる

### apply command

まずはコマンドのmain部分から。ここでのcとはcommand.ApplyCommandのこと。多くの機能はcommand.Metaに実装されている。

```go
// ApplyCommand is a Command implementation that applies a Terraform
// configuration and actually builds or changes infrastructure.
type ApplyCommand struct {
	Meta

	// If true, then this apply command will become the "destroy"
	// command. It is just like apply but only processes a destroy.
	Destroy bool
}

```

ログ。

```
2021/6/5 22:40 [$HOME/ghq/github.com/hashicorp/terraform/command/apply.go]
c.RunOperation()が本体っぽい

2021/6/5 22:40 [$HOME/ghq/github.com/hashicorp/terraform/command/apply.go]
c.BackendForPlan()でいい感じにbackendを取り出す

2021/6/5 22:39 [$HOME/ghq/github.com/hashicorp/terraform/command/apply.go]
planの取り出しはplanFile.ReadPlan()

2021/6/5 22:38 [$HOME/ghq/github.com/hashicorp/terraform/command/apply.go]
planの実行はc.PlanFile()

2021/6/5 22:38 [$HOME/ghq/github.com/hashicorp/terraform/command/apply.go]
とりあえずこのコマンドが実行される
```

ctyから情報を読み取る分にはplanを見れば良いはずなのでplanを見よう。

### plan.Open

本質的には、terraform/plans/planfile.Open() っぽい。

```
2021/6/5 22:47 [plans]
EvalContext()とかはどのあたりで使われるんだろう？というのが謎と言う感じか。

2021/6/5 22:46 [$HOME/ghq/github.com/hashicorp/terraform/plans/planfile/tfplan.go]
valueFromTfplanとかがよくわかっていないな。

2021/6/5 22:45 [$HOME/ghq/github.com/hashicorp/terraform/plans/planfile/tfplan.go]
planproto.Plan()が使われている。

2021/6/5 22:45 [$HOME/ghq/github.com/hashicorp/terraform/plans/planfile/tfplan.go]
readTfplanってproto.Unmarshal()がでてくるけれどprotobuf使っているのか。

2021/6/5 22:45 [$HOME/ghq/github.com/hashicorp/terraform/plans/planfile/reader.go]
ReaderのReadPlan()も本質的にはreadTfplan()だった。

2021/6/5 22:44 [$HOME/ghq/github.com/hashicorp/terraform/plans/planfile/reader.go]
Open()はreaderを作っているだけか
```

### eval context

これを見れば良いのか

- https://github.com/hashicorp/terraform/blob/main/docs/architecture.md#

なるほど、terraform.Contextが重要で見ていけば良い。

あとはbackend.Operationが実行。

```
2021/6/5 22:59 [$HOME/ghq/github.com/hashicorp/terraform/internal/terraform/context.go]
さらにGraphTypeで分岐しているんだなー。これが実行系っぽい。GraphTypeApply..

2021/6/5 22:58 [$HOME/ghq/github.com/hashicorp/terraform/internal/backend/local/backend_apply.go]
opWait(doneCh,stopCtx,canclCtx,tfCtx,opState,op.View) いい

2021/6/5 22:57 [$HOME/ghq/github.com/hashicorp/terraform/internal/backend/local/backend_apply.go]
context.Apply()を実行している感じ。

2021/6/5 22:57 [$HOME/ghq/github.com/hashicorp/terraform/internal/backend/local/backend_apply.go]
tfCtxを取り出して、stateを取り出す

2021/6/5 22:56 [$HOME/ghq/github.com/hashicorp/terraform/internal/backend/local/backend.go]
local backendを見るのが一番手軽そう。backend.OperationTypeApplyをどう扱っているのか
```

さらに

```
2021/6/5 23:21 [$HOME/vboxshare/sandbox/go/pkg/mod/github.com/hashicorp/hcl/v2@v2.8.2/hcldec/spec.go]
実際にObjectをどう扱っているかはここに書かれていた。

2021/6/5 23:18 [$HOME/vboxshare/sandbox/go/pkg/mod/github.com/hashicorp/hcl/v2@v2.8.2/hcldec/spec.go]
ここにBlockObjectSpec等がある。

2021/6/5 23:18 [$HOME/vboxshare/sandbox/go/pkg/mod/github.com/hashicorp/hcl/v2@v2.8.2/hcldec/spec.go]
AttrSpecであることが多いみたい。最初は。

2021/6/5 23:18 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/configschema/decoder_spec.go]
Specはinterfaceで実際はdecodeSpecで取り出している。

2021/6/5 23:16 [$HOME/vboxshare/sandbox/go/pkg/mod/github.com/hashicorp/hcl/v2@v2.8.2/hcldec/spec.go]
定義されているinterface。hcl.BodyContentをcty.Valueにするもの。なので先程の実行だね。

2021/6/5 23:16 [$HOME/vboxshare/sandbox/go/pkg/mod/github.com/hashicorp/hcl/v2@v2.8.2/hcldec/decode.go]
ここでhc.BodyからContentを取り出してspec.Decodeしている。ここでのspecって何かというと

2021/6/5 23:15 [$HOME/ghq/github.com/hashicorp/terraform/internal/lang/eval.go]
scopeはinternal/langにあるものこれでつながった、ここでhcldecのDecodeをしている。ここが知りたかった。

2021/6/5 23:14 [$HOME/ghq/github.com/hashicorp/terraform/internal/terraform/eval_context_builtin.go]
BuiltinEvalContextがその実装で、内部的にはctx.EvaluationScopeでscopeをとりだしてから、scope.EvalBlock()している。

2021/6/5 23:13 [$HOME/ghq/github.com/hashicorp/terraform/internal/terraform/graph_walk_context.go]
ContextGraphWalkerのEvalContext()でEvalContextが作られる

2021/6/5 23:12 [$HOME/ghq/github.com/hashicorp/terraform/internal/terraform/graph.go]
EvalContext()自体はgraphをwalkしているときに呼びそう。

2021/6/5 23:11 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/module_call.go]
decodeというのはhc.Blockをとってconfigs.ModuleCallを取るようなこと。

2021/6/5 23:10 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/module_call.go]
sourceがどう読まれているかはこの辺を読んでいけば良いのか。

2021/6/5 23:10 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/parser_config.go]
めちゃくちゃ素直に case "module": のときにはdecodeModuleBlock()を呼ぶとか書かれている

2021/6/5 23:9 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/parser_config.go]
型はhcl.Body

2021/6/5 23:9 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/parser_config.go]
LoadHCLFile()でとりだして

2021/6/5 23:8 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/parser_config_dir.go]
LoadConfigFile()

2021/6/5 23:8 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/parser_config_dir.go]
LoadConfigDir()

2021/6/5 23:8 [$HOME/ghq/github.com/hashicorp/terraform/internal/configs/configload/loader_load.go]
この辺読んでいくのが正しそう。
```

## まじめにterraformのmoduleのvariableが部分型で十分な理由を調べる

- http://github.com/hashicorp/terraform/configs/  あたりを見て
- http://github.com/zclconf/go-cty/cty あたりを見ればわかりそうな気がする。

memo

- parse modelは HCL
- `terraform/configs.ModuleCall` がmoduleの呼び出し部分
- terraform/lang/eval.goなどを読んでいく？ 全体のphaseがわかんないとあんまり良くないな

### go-ctyのドキュメントを読んで見る

https://github.com/zclconf/go-cty/blob/main/docs/concepts.md

- types

  - primitive types (e.g. string, number, ..)
  - compound types

    - collection types (e.g. list, map)
    - structural types (e.g. object, tuple)

https://github.com/zclconf/go-cty/blob/main/docs/types.md
https://github.com/zclconf/go-cty/blob/main/docs/convert.md

### 追記

した https://pod.hatenablog.com/entry/2021/06/05/201955
