## golang mage使ってみる

```
go get -u -v -d github.com/magefile/mage
cd $GOPATH/src/github.com/magefile/mage
go run bootstrap.go
```

:thought-balloon: とりあえず動かしてみる

- サイト覗く
- なんか動かしてみる
- pluginというものがあるらしい
- 中覗く

`MAGE_VERBOSE=1` というもの良さそう。

### hello world

initのオプションはある。あー。何かmain()を挿入して作るみたいな感じっぽいな。
(色々標準のタスクが生成される）

```
$ merge -init
```

以下でtargetの一覧見れるの良いな

```
$ merge -l
```

以下のようなmagefile.goを作ってみる。

```go
// +build mage

package main

import (
	"fmt"
)

// echo bye
func Bye() {
	fmt.Println("bye")
}

// echo hello
func Hello() {
	fmt.Println("hello")
}

var Default = Hello
```

DefaultがDefaultタスクっぽい。

:thought-balloon: 色々lint系のエラーが出るのだるいな。
(flycheckを特定のファイル名のものだけ切るような実装にする？)

雑な対応

```lisp
(defun my:go-flycheck-mode ()
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (unless (string-equal filename "magefile.go")
      (flycheck-mode)
      ))
  )

(add-hook 'go-mode-hook 'my:go-flycheck-mode)
```

### 引数を渡す方法

たぶん存在していなくて環境変数を見たりするくらいしか無い。

### cacheの使いかた

file sourceのtaskは `mage/target` に逃してる。(taskじゃなくてtargetと呼ぶようだけれど)

### aliases

```go
var Aliases = map[string]interface{} {
  "i":     Install,
  "build": Install,
  "ls":    List,
}
```

### target定義の方法

```
func()
func() error 
func(context.Context)
func(context.Context) error
```

### 依存関係

magefile/mage/mg 付近にhelperがあるらしい。(`mg.Deps`, `mg.SerialDeps`)


