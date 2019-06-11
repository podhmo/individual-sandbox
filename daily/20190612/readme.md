## python cookiecutter

- https://github.com/audreyr/cookiecutter

依存。

```
cookiecutter==1.6.0
  - binaryornot [required: >=0.2.0, installed: 0.4.4]
    - chardet [required: >=3.0.2, installed: 3.0.4]
  - click [required: >=5.0, installed: 7.0]
  - future [required: >=0.15.2, installed: 0.17.1]
  - jinja2 [required: >=2.7, installed: 2.10]
    - MarkupSafe [required: >=0.23, installed: 1.0]
  - jinja2-time [required: >=0.1.0, installed: 0.2.0]
    - arrow [required: Any, installed: 0.12.1]
      - python-dateutil [required: Any, installed: 2.7.3]
        - six [required: >=1.5, installed: 1.11.0]
    - jinja2 [required: Any, installed: 2.10]
      - MarkupSafe [required: >=0.23, installed: 1.0]
  - poyo [required: >=0.1.0, installed: 0.4.2]
  - requests [required: >=2.18.0, installed: 2.19.1]
    - certifi [required: >=2017.4.17, installed: 2018.4.16]
    - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
    - idna [required: >=2.5,<2.8, installed: 2.7]
    - urllib3 [required: >=1.21.1,<1.24, installed: 1.23]
  - whichcraft [required: >=0.4.0, installed: 0.5.2]
```

cookiecutterの機能ってどこまであるんだっけ？

- templateを選んで良い感じにscaffold
- templateの種類がlocal,netの２つ
- 事前に設定状況をJSONにまとめて渡せる(はず)
- その場でたずねて設定状況を埋められる(はず)

これくらいの認識。

- https://cookiecutter.readthedocs.io/en/latest/

### 気になること

- バイナリファイルの追加
- coockiecutterをpythonから呼ぶことの意義ってあるんだろうか？
- configに渡すのは `cookiecutter.json + --no-input`

  - 他も含めて色々デフォルトにしたい場合には `~/.cookiecutterrc`

### 簡単なtemplateの作り方

- 設定はcookiecutter.json
- default_contextとextra_context
- (extra helpers的なものが欲しいのだけれど)
- https://github.com/audreyr/cookiecutter/pull/944
- https://github.com/audreyr/cookiecutter/blob/master/cookiecutter/environment.py#L49
- https://github.com/pallets/jinja/blob/master/jinja2/utils.py#L124

### python jinja2 実はextensionでadditional modules対応できない？


## openapi go openapi3で無難なもの

https://github.com/deepmap/oapi-codegen

気にするべきはここ。けっこうruntimeにまかせているので型としては強くない。

- https://github.com/deepmap/oapi-codegen/tree/master/examples/petstore-expanded

ただし不足している機能は多いので快適には使えないかも(e.g. oneOf,anyOf,allOf,patternProperties,additionalProperties)

### validationなどにもしかしたら

- https://github.com/wework/speccy
