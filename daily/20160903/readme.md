# emacs 関数のコードをリストとして取得

```lisp
(let (print-level print-depth)
  (pp (symbol-function 'flycheck-start-command-checker)))
```

# wip golang rate limitの方法

まず使用方法を調べる。その後実装を見てみる


# golang ツール作成関係は以下を見ると色々身につきそう？

- [Go でいい感じのコマンドを作れるツールキットの紹介 - Cybozu Inside Out | サイボウズエンジニアのブログ](http://blog.cybozu.io/entry/cybozu-go-cmd)
- [Tutorial · cybozu-go/cmd Wiki](https://github.com/cybozu-go/cmd/wiki/Tutorial)

## tutorial

[tutorial](https://github.com/cybozu-go/cmd/wiki/Tutorial) の部分を参考にしていく。

もしかしてdecorator objectっぽい感じになっているんだろうか？


# golang mac goを1.7にあげた

```bash
$ sudo port selfupdate
$ sudo port upgrade go
```

# memo 過去のreadmeも見かえすと良い。

channelを使ったsyncの部分とか復習したほうが良さそう。

# vagrantでちょっとしたlinux環境はあってもよいか

- https://atlas.hashicorp.com/bento/boxes/ubuntu-16.04

```bash
cd ~/vms
vagrant init bento/ubuntu-16.04; vagrant up --provider virtualbox
```
