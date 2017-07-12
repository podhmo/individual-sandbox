# python notebook headless jupyterはnbconvertを使えば良い

`--execute` を付けると真面目に実行してくれる。

```
jupyter nbconvert --to notebook --output <name> --ExecutePreprocessor.timeout=600 --execute <filename>.ipynb
```

色々なフォーマットに対応しているっぽい `--to`

- asciidoc
- custom
- html
- latex
- markdown
- notebook
- pdf
- python
- rst
- script
- slides

# python httpie パラメーターの渡し方

|effect|example|
|:--|:--|
|header|`Origin:example.com`|
|post data|`name="John Doe"`|
|query string|`q=="search"`|
|json|`list:='[1,2,3]'`|
|read from file(json)|`user:=@user.json`|
