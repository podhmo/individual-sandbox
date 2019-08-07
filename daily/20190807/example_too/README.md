## やりたいこと

1. serverを実行
2. clientを実行
3. client実行後に良い感じでserverを終了
4. (client, server共に特殊な仕掛けを用いない。コードが触れることも期待しない)

これを `ioknife too --cmd <server> --cmd <client>` みたいな形での実行で対応したい。
(client側はmake taskになるかも。全体もmake task)

### 補足事項的なもの

- pgid指定してのSIGTERMの実行はステータスが0以外になって嬉しくない(`kill -TERM -<pgid>`)
- イメージとしてはgraceful stopに近い
- これとは別件でrestart(with reload)的な機能も欲しい

## 調査

### make経由での実行

pgidはmakeのものが引き継がれるっぽい。

01

```console
$ make 01
cat 01cmds.txt | ioknife too
[0] python   tick 0
[1] python   PID PPID PGID PGRP
[1] python   PID PPID PGID PGRP
[1] python   13797 13795 13792 13792
[1] python   13797 13795 13792 13792
[0] python   tick 1
[1] python   PID PPID PGID PGRP
[1] python   13797 13795 13792 13792
...
```

pstree

```console
$ pstree -T -p -g -A 13792
make(13792,13792)---sh(13793,13792)---ioknife(13795,13792)-+-python(13796,13792)
                                                           `-python(13797,13792)

```

make経由で実行するとpgidはmakeのものになるなー。ioknifeにsignalをsendということができていないかも？

### やりたいこと(?)

- SIGHUPでrestart?
- SIGTERM(?)で子プロセスを全部terminateしてから自分自身の終了プロセスを走らせる？

でもこれはioknifeのpidを指定する必要があって微妙に面倒？(pgid常にという方が楽そうではあったのだけれど。ppidを使うということになるとちょっとめんどくさそう。例えばioknife上でmakeを実行したときとかどうなるんだろう？)

### makeの実行も込みでやってみた。

まぁそうなるよねという感じ。

```console
$ pstree -p -g -A 14091
make(14091,14091)---sh(14092,14091)---ioknife(14094,14091)-+-make(14096,14091)---python(14097,14091)
                                                           `-python(14095,14091)
```

### 分かっていないこと

分かっていないことは以下

- 停止のtriggerのsignalに何を使うか(SIGHUP?SIGTERM)
- subprocessでコマンドを実行したときの停止
- subrpocessでシェルを実行したときの停止
- subrpocessでmake経由でコマンド実行したときの停止(間にshが挟まる)
- ioknife自身を直接ターミナルから実行したとき
- ioknife自身をmake経由で実行したとき

このときの

- それぞれにSIGHUPを送った場合
- それぞれのpgidにSIGHUPを送った場合

きれいに伝搬されるだけがうれしいのだけれど。


## あとで読み返す用のメモ

- https://pod.hatenablog.com/entry/2019/08/07/163409
- http://shinpeim.github.io/process-book/008.md/
- https://github.com/podhmo/ioknife
- https://unix.stackexchange.com/questions/149741/why-is-sigint-not-propagated-to-child-process-when-sent-to-its-parent-process
- http://veithen.io/2014/11/16/sigterm-propagation.html
- https://github.com/Supervisor/supervisor
