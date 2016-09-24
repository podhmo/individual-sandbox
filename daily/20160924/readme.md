# shell mac macのディスクの容量がまずい。

どのファイルを消すか調べよう。

```
$ du -sh /* | sort -n
```

xxxMとかxxxGとかの表示になっていないと人間は把握しづらいがソートできない。

いろいろ考えた結果が以下。

```
$ sudo du -h / > /tmp/mem.txt  # 60Mbくらい
$ cat /tmp/mem.txt | grep -P '^ *([\d\.]+G|\d{3,}M)' | gsort -h
```

## duのこと

`-s` をつける必要はなく全部のディレクトリを表示対象にして良いのでは？

## sortのこと

coreutilsの方なら `-h` オプションがある。

```
sudo port install coreutils
gsort -h
```

## grepのこと

巨大なものだけを見たい。はじめは素のgrepを使った。辛い。

```
awk '{print $1,$2}' | grep '[0-9][0-9\.]*\(G\|M\)'
```

せめて `-E` オプションを。(本当のこと言うとperl由来の正規表現を使いたい)。

```
awk '{print $1,$2}' | grep -E "^[0-9\.]+(G|M)"
```

gnu grepは別途installする必要があった。
そして、sortやgrepで行頭の空白に対応すればawkは不要だった。

```
sudo port install grep
grep -P '^ *([\d\.]+G|\d{3,}M)'
```

# swagger jsonschema swagger用のsourceを手軽にいじるやつ

## jsonschemaでswaggerのformatをチェックできない？

:tada:

- http://json.schemastore.org/swagger-2.0

## jsonschemaをみてjsonをsyntax checkできない？

flycheckに渡したい。

## yamlで書いているjsonをjsonschemaでsyntax checkできない？

変換前後の行数を保持していないとダメな感じはしている。

# make `make -p` 忘れがちなのでメモ

```
%.py.err.txt: %.py
	python $< 2>&1 | sed "s@$$HOME@~@g" > $@

default: $(shell find . -name "*check.py" | sed 's@$$@.err.txt@g' | tr '\n' ' ')

clean:
	rm -f *.err.txt

.PHONY: clean default
```

## この辺忘れる

- [GNU make: Automatic Variables](https://www.gnu.org/software/make/manual/html_node/Automatic-Variables.html)

```
all: library.cpp main.cpp

In this case:

    $@ evaluates to all
    $< evaluates to library.cpp
    $^ evaluates to library.cpp main.cpp
```
