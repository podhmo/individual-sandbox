## golang go中覗く

どこから覗いていこう？

- ディレクトリの構造を把握
- ビルド部分を把握
- cacheしたい

### ビルド部分を把握

昔一度把握した気がするけれど忘れてしまっている。

```
cd src
GOOS=os GOARCH=arch ./bootstrap.bash
```

やっていることを雑に書くと以下のような感じ

```
targ="../../go-${GOOS}-${GOARCH}-bootstrap"
src=$(cd .. && pwd)
cp -R "$src" "$targ"  # src以下を特定のディレクトリにコピー
./make.bash --no-banner
```

make.bash

```
# 色々チェック
# bootstrapのgoを使う感じ

GOROOT="$GOROOT_BOOTSTRAP" GOOS="" GOARCH="" "$GOROOT_BOOTSTRAP/bin/go" build -o cmd/dist/dist ./cmd/dist
./cmd/dist/dist bootstrap $buildall $GO_DISTFLAGS -v # builds go_bootstrap
CC=$CC_FOR_TARGET "$GOTOOLDIR"/go_bootstrap install $GO_FLAGS -gcflags "$GO_GCFLAGS" -ldflags "$GO_LDFLAGS" -v std cmd
```

結局、`go install`をbootstrapしたgoで呼ぶ感じ。distだけどういうコマンドなのか把握したほうが良い感じ。


> This program, dist, is the bootstrapping tool for the Go distribution.
> The process to install Go 1.x, for x ≥ 5, is:
>
> 1. Build cmd/dist with Go 1.4.
> 2. Using dist, build Go 1.x compiler toolchain with Go 1.4.
> 3. Using dist, rebuild Go 1.x compiler toolchain with itself.
> 4. Using dist, build Go 1.x cmd/go (as go_bootstrap) with Go 1.x compiler toolchain.
> 5. Using go_bootstrap, build the remaining Go 1.x standard library and commands.

see

- https://golang.org/doc/install/source

### ディレクトリの構造を把握

ディレクトリ

|name|description|
|:--|:--||
|./lib||
|./src||
|./api||
|./.github||
|./doc||
|./test||
|./misc||

ディレクトリ(level=2)

```
.
./lib
./lib/time
./src
./src/fmt
./src/strings
./src/internal
./src/runtime
./src/index
./src/archive
./src/hash
./src/crypto
./src/log
./src/net
./src/syscall
./src/cmd
./src/sort
./src/compress
./src/strconv
./src/unicode
./src/database
./src/sync
./src/debug
./src/plugin
./src/regexp
./src/errors
./src/container
./src/go
./src/bufio
./src/math
./src/context
./src/html
./src/unsafe
./src/path
./src/image
./src/builtin
./src/bytes
./src/vendor
./src/reflect
./src/io
./src/mime
./src/text
./src/time
./src/os
./src/flag
./src/expvar
./src/encoding
./api
```
