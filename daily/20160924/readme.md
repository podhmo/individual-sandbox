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
