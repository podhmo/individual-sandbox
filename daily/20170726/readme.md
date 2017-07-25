## task runnerを見繕いたい

なかったら作る

- [go-godo/godo: golang build tool in the spirt of rake, gulp](https://github.com/go-godo/godo)
- [go-task/task: Simple task runner / Make alternative written in Go](https://github.com/go-task/task)
- [tj/robo: Simple Go / YAML-based task runner for the team.](https://github.com/tj/robo)

直感的には、 `task >= robo >> godo` 位の感じ

他にも https://github.com/go-task/task#alternative-task-runners に代替があったりする。

### 思ったこと

- yamlベースの方がgoのコードで書くよりは良いと思うっぽい
- できればglobal taskが一覧表示されると良い
- concurrency的なものもサポートしていると嬉しい
- たぶんfile単位ではない形でcache/skip的な何かをしたい

## go go-task 試す

install

```
$ go get -u -v github.com/go-task/task/cmd/task
```

init

```
$ task --init
```

default taskの実行

```
$ task
task: No argument given, trying default task
echo "Hello, World!"
Hello, World!

$ task defefault
echo "Hello, World!"
Hello, World!
```

### taskの一覧表示

```
$ task -l
Available tasks for this project:
* bar:	*bar*
* foo:	*foo*
```

ただし `desc` をつけてあげないとダメ。

```
foo:
  desc: "*foo*"
  cmds:
    - echo foo
bar:
  desc: "*bar*"
  cmds:
    - echo foo
```


