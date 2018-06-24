## python pydoc

以下の様にすると、venvの環境を候補にpydocを開ける

```
＄ python -m pydoc <module>
＄ python -m pydoc -b matplotlib
```

## makefile いい感じに出力

```make
dist/%.out: %.py
	mkdir -p dist
	python $^ | tee $@

default: $(addprefix dist/,$(addsuffix .out,$(basename $(wildcard *.py))))

clean:
	rm -rf dist
```
