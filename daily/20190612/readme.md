## python tweenのuploadで以下の様なメッセージがでる

```console
$ twine upload --verbose dist/kamidana-0.8.0-py2.py3-none-any.whl
Uploading distributions to https://upload.pypi.org/legacy/
Uploading kamidana-0.8.0-py2.py3-none-any.whl
 25%|█████████████████▌                                                    | 8.00k/32.0k [00:0100%|█████████████████████████████████████████████████████ █████████████████| 32.0k/32.0k [00:01<00:00, 19.5kB/s]
Content received from server:
<html>
 <head>
  <title>400 The description failed to render in the default format of reStructuredText. See https://pypi.org/help/#description-content-type for more information.</title>
 </head>
 <body>
  <h1>400 The description failed to render in the default format of reStructuredText. See https://pypi.org/help/#description-content-type for more information.</h1>
  The server could not comply with the request since it is either malformed or otherwise incorrect.<br/><br/>
The description failed to render in the default format of reStructuredText. See https://pypi.org/help/#description-content-type for more information.


 </body>
</html>
HTTPError: 400 Client Error: The description failed to render in the default format of reStructuredText. See https://pypi.org/help/#description-content-type for more information. for url: https://upload.pypi.org/legacy/
```

renderに失敗しているということなので、特に[ヘルプメッセージの指すリンク先](https://pypi.org/help/#description-content-type)をみて`description-content-type`を指定しても意味がない。

ちなみに、`text/x-rst`に変えた場合のメッセージは以下の様になる。

```
HTTPError: 400 Client Error: The description failed to render for 'text/x-rst'. See https://pypi.org/help/#description-content-type for more information. for url: https://upload.pypi.org/legacy/
```

解決にはこのPRが参考になった。

https://github.com/zalando-incubator/Transformer/pull/60/files

```console
$ pip freeze | grep -i readme
readme-renderer==24.0
```

これだけのwarningでもだめ。

```console
$ python -m readme_renderer README.rst
<string>:417: (WARNING/2) Title underline too short.

available info (extensions and additional modules)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

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
