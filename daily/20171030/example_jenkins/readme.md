jenkinsで表示したい感じなのでxunitの形式で保存してみる

- https://dev.classmethod.jp/testing/unittesting/junit-xml-format/


## jenkinsの環境作ってみる

```
$ docker pull jenkins
$ docker run -p 8080:8080 -p 50000:50000 jenkins
```

build

```
curl https://gist.githubusercontent.com/podhmo/94b900b1f50b785309bdadb84daba854/raw/22d6ddd055198e22b7b41981c3bd0f75a6e46c0a/report.xml > report.xml
```


