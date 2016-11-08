# git 追随忘れていたbranchの追随にgit rebaseを使う

直接使うとあれなのでontoを使うし。現在のbranchに直接変更を加えると怖いので別のbranchでやる。

以下のようなrevisionの状態

```
master: a -> b -> c -> d -> e
f0                c -> x -> y -> z
```

ここから `y -> z` だけ取り出したい(xは捨てる)

```
git co master
git co -b tmp-master
git co f0
git co -b tmp-f0
git rebase --onto tmp-master x tmp-f0
git co f0
git reset --hard tmp-f0
git br -D tmp tmp-f0
```
