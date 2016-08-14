# pythonで外部コマンドの実行

わりと幾つか方法あるし。幾つかはdeprecated。やる気出すならasyncioのやつ使っても良い。

[subprocess](https://docs.python.org/3/library/subprocess.html)使っとくのが無難。

```
po = subprocess.run("ls -l", shell=True, check=True, stdout=PIPE)
print(po.stdout.decode("utf-8").rstrip())
```

shellとcheckはdefaultでTrueにしておくと良いんじゃないかな。


# golangの `go list` が結構便利

jsonで色々な情報を出力

```bash
$ go list -json fmt
```

text templateの構文で出力結果を調整できる。

```bash
$ go list -f '{{ .Deps }}' fmt
$ go list -f '{{ .Imports }}' fmt
```

全てのパッケージを表示(all)

```bash
$ go list all
```

全ての標準ライブラリのパッケージを表示(std)

```bash
$ go list std
```
