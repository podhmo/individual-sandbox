configからopenapi spec(の一部)を生成

```console
$ swaggerknife json2swagger data/00/*.json --name person --dst src/person.json --annotate data/annotation/00.json
$ swaggerknife json2swagger data/01/*.json --name db --dst src/db.json #--annotate data/annotation/01.json
```

作られたspecをmergeしたmainの作成

```console
$ swaggerknife merge `ls src/*.json | grep -v main.json` --style ref --dst src/main.json
```
