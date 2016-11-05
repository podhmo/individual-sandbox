# golang importsされるものまで取り出したいかも

どうするのだっけ？

- [example_parser](./example_parser)

# golang go runでxxx.goを引数に渡す方法

以下の様なことがしたい

```
go.run main.go xxx.go
```

build対象だと勘違いされる。

## とりあえずflagパッケージ使うことにする

flag packageに利用した変数はmain()以外で触りたくないみたいな気持ちがあるので以下の様に書く

```go
package main

import "flag"

var target *string

func init(){
	target = flag.String("target", "", "target file")
}

func main(){
	flag.Parse()
	run(*target)
}

func run(fname string){
	// 実際の処理
}
```
