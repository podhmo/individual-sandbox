#[make][bash][memo] makefileのforeachのハマりどころ

タスクランナーとしてのmakeのN回目。あんまり深追いしたくはないという気持ちもありつつ。

[以前書いたように](https://pod.hatenablog.com/entry/2017/06/13/150342)makefile中ではbashの関数などの定義ができない。そんなわけでdefineとcallを使う。これはこれで便利。ただforeachまで使おうとするとちょっとハマるかもというポイントがあるのでメモ。

### define,callを使って処理をまとめる

```make
# この定義はforeachを使う所で上手くいかなくなるので注意(良くない例)
define F =
	echo $(1)
	echo $(1)
endef

f0:
	$(call F,foo)
	$(call F,bar)
```

これは以下の様に展開される。

```
$ make -n f0
echo foo
echo foo
echo bar
echo bar
```

なるほど。良い。

### foreachを使いたい

さて、Makefileで複数の対象を候補に同じ処理を呼びたいということもある。つまるところ対象としてlistを指定したい。このようなときにはforeachを使う。使うのだけれど。これに少しだけハマりどころが存在する(こういうところがあるのでMakeの知識はバッドノウハウっぽい。本当は代替のタスクランナーがあれば乗り換えたい。ただしできればインストールにパッケージマネージャが不要なものが良い(LLなどはこのあたりで候補から外れる))。

以下の様にリスト(xs)を用意して、先程定義したFをそのリストに対して使おうとしてみる。

```make
# この定義はforeachを使う所で上手くいかなくなるので注意(良くない例)
define F =
	echo $(1)
	echo $(1)
endef

xs := foo bar
f1:
	$(foreach x,$(xs),$(call F,$(x)))
```

`-n` で実際に実行されるスクリプトを見てみると、一部期待通りではない形になっている。もちろん当然ではあるけれど。このようなコードではfooでの末尾を実行したタイミングで不用意に引数としてbarの先頭行が渡されるということになり。エラーが出てしまう。

```
$ make f1 -n
echo foo
echo foo        echo bar
echo bar
```

Makefileのdefineはテキストを定義するというだけの意味なので。endefの手前までの部分、つまるところ改行を含まない定義がforeachによって呼ばれるのでダメ。

期待通りに書くには以下の様にする必要がある。

```make
# こちらは期待通りに動く
define F =
	echo $(1)
	echo $(1)

endef

xs := foo bar
f2:
	$(foreach x,$(xs),$(call F,$(x)))
```

最後に１行改行が必要。こうすると上手くいく。

```
$ make f2 -n
echo foo
echo foo
echo bar
echo bar

```

## foreachが使えるようになるとできること

ちなみにforeachが使えるようになるとできることが色々ある。wildcardとbasename,addsuffixあたりを組み合わせると便利。
（あんまりmakeのことを覚えても仕方がないというところもあるので`$(shell ...)` やバッククォートによるコマンドの実行を使った形の方が共有はしやすいかもしれない）

例えば、`setup.py`を持つものをpython packageだとすると。これらの全部のテストを実行するには以下で良い。

```make
define testT =
	(cd $(dir $(1)) && python setup.py -q test)

endef

testall:
	$(foreach p,$(wildcard */setup.py),$(call testT,$(p)))
```

実際に実行してみる

```
$ make -n
(cd foo/ && python setup.py -q test)
(cd bar/ && python setup.py -q test)

$ make
(cd foo/ && python setup.py -q test)

----------------------------------------------------------------------
Ran 0 tests in 0.000s

OK
(cd bar/ && python setup.py -q test)

----------------------------------------------------------------------
Ran 0 tests in 0.000s

OK
```

注意点として、makeでのコマンドは1行毎に違うシェルで実行されるので(正確な表現ではないけれど)以下ではダメ。

```
define testT =
	cd $(dir $(1))
    python setup.py -q test  # cdする前の位置で実行される
    cd ..

endef
```
