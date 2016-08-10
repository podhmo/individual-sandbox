# pythonで外部コマンドの実行

わりと幾つか方法あるし。幾つかはdeprecated。やる気出すならasyncioのやつ使っても良い。

[subprocess](https://docs.python.org/3/library/subprocess.html)使っとくのが無難。

```
po = subprocess.run("ls -l", shell=True, check=True, stdout=PIPE)
print(po.stdout.decode("utf-8").rstrip())
```

shellとcheckはdefaultでTrueにしておくと良いんじゃないかな。
