## golang dep

setup

```
go get -v -u github.com/golang/dep/cmd/dep
dep init -v
dep ensure -v -update
```

initで生成されるもの

- Gopkg.lock
- Gopkg.toml
- vendor (directory)

Gopkg.toml は設定ファイルのようなもの。

```toml
# Gopkg.toml example
#
# Refer to https://github.com/golang/dep/blob/master/docs/Gopkg.toml.md
# for detailed Gopkg.toml documentation.
#
# required = ["github.com/user/thing/cmd/thing"]
# ignored = ["github.com/user/project/pkgX", "bitbucket.org/user/project/pkgA/pkgY"]
#
# [[constraint]]
#   name = "github.com/user/project"
#   version = "1.0.0"
#
# [[constraint]]
#   name = "github.com/user/project2"
#   branch = "dev"
#   source = "github.com/myfork/project2"
#
# [[override]]
#  name = "github.com/x/y"
#  version = "2.4.0"


[[constraint]]
  branch = "master"
  name = "github.com/podhmo/hatena"
```

Gopkg.lockはversion lock。こちらが重要なのかも。

### よく分かっていないところ

`dep init` 後に `dep ensure`　しても新しいpackageがfetchされてくるっぽい。

dep initで生成されたlockはトップレベルのものだけなのかもしれない。
