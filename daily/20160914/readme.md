# python

constraint fileの説明したほうが良いのかな？

- [User Guide — pip 8.1.2 documentation](https://pip.pypa.io/en/stable/user_guide/#constraints-files)

`pip install -r requirements.txt -c constraints.txt --cache-dir=/tmp <package>`


# wip golang goaを試してみる

- 中の関係とか見てみる
- getting started 試す

## wip getting started 試す

- [goa :: Design-first API Generation](https://goa.design/learn/guide/)

```
$ go get -v github.com/goadesign/goa/...
$ mkdir -p $GOPATH/src/github.com/podhmo/cellar/design
$ editor $GOPATH/src/github.com/podhmo/cellar/design/design.go
$ $GOPATH/src/github.com/podhmo/cellar
$ goagen bootstrap -d github.com/podhmo/cellar/design
$ mkdir -p bin
$ go build -o bin/cellar
```

# shell 手抜きのreplace

```
function replace () { grep -lr $1 . --exclude=.git | xargs gsed -i "s@$1@$2@g"; }
function replace2 () { echo "grep -lr \"$1\" .  --exclude=.git | xargs gsed -i \"s@$1@$2@g\";"; }
```

# git aliasを手軽に利用したい

- [これ](http://qiita.com/peccul/items/90dd469e2f72babbc106)

# firefox vimperatorのcopy.js

```
cd ~/.vimperator
mkdir plugin
curl https://gist.githubusercontent.com/podhmo/893357200a8cfa78514705a1521c3989/raw/e1dd7482fc4b216b6698bce44f022e6685941de4/copy.js > plugin/copy.js
```

firefoxのところで `:loadplugins` を実行
