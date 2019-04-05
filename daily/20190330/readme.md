## やりたいこと

- lsp-modeを覗く
- mylspのサンプルを増やす
- json/yamlのparseエラーで行番号を取得する
- json/yamlのvalidationエラーで行番号を取得する
- dictknifeをイジる

## mkdictを組み込んだshell
(outputは？)


## myslpのサンプルを増やす

- code completion
- goto definition
- diagnosticsの対応

### code completion

https://microsoft.github.io/language-server-protocol/specification#textDocument_completion

capabilityは `completionProvider`。

もしかしてendpointもう少しきれいに作れるのでは？

- textDocument/completion
- (completionItem/resolveというendpointもある？)

⚠ socketでつなぐときにも毎回initは呼ばないとダメらしい(python_ls)

```request
{
  "jsonrpc": "2.0",
  "id": "1655d4e6-0178-448c-b178-e6b382b318ad",
  "method": "textDocument/completion",
  "params": {
    "textDocument": {
      "uri": "file://VENV/individual-sandbox/daily/20190330/example_mylsp/something.py"
    },
    "position": {
      "line": 1,
      "character": 7
    }
  }
}
```

response

```json
{'id': '123f8aa5-87d0-48d5-abb1-fcfad19b2d09',
 'jsonrpc': '2.0',
 'result': {'isIncomplete': False,
            'items': [{'detail': 'sys',
                       'documentation': 'exit([status])\n'
                                        '\n'
                                        'Exit the interpreter by raising '
                                        'SystemExit(status).\n'
                                        'If the status is omitted or None, it '
                                        'defaults to zero (i.e., success).\n'
                                        'If the status is an integer, it will '
                                        'be used as the system exit status.\n'
                                        'If it is another kind of object, it '
                                        'will be printed and the system\n'
                                        'exit status will be one (i.e., '
                                        'failure).',
                       'insertText': 'exit(${1:status})$0',
                       'insertTextFormat': 2,
                       'kind': 3,
                       'label': 'exit(status)',
                       'sortText': 'aexit'}]}}
```

### signature

- textDocument/signatureHelp
- textDocument/hover
- textDocument/definition
- textDocument/documentHighlight

methodが変わるだけかも

```
client-request (id:6) Sat Mar 30 16:42:45 2019:
(:jsonrpc "2.0" :id 6 :method "textDocument/signatureHelp" :params
          (:textDocument
           (:uri "file://VENV/individual-sandbox/daily/20190330/example_golsp/main.go")
           :position
           (:line 5 :character 11)))
```

hmm

```
method  "textDocument/signatureHelp" params { textDocument/uri "something.py" position { line 1 character 2 } }
```

## python pathlib覚える

```python
import pathlib

root_uri = pathlib.Path(__file__).parent.absolute().as_uri()
```
