<<<<<<< HEAD
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
=======
## docker 復習

- image
- container

imageの一覧

```console
$ docker images
```

containerの一覧

```console
$ docker ps

# 全部
$ docker ps -a
```

containerの削除

```console
$ docker rm <container id>
```

imageの削除

```console
$ docker rmi <image id>
```

volume系のなにか

```console
$ docker volume inspect `docker volume ls -q | head -n 1`
```


>>>>>>> 68b32a0... -
