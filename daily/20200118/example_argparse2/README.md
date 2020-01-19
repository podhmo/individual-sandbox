# argparseでdictと仲良くしたい

## argparseとJSONだけの場合

設定の検証ができない

## pydantic

設定の検証に[pydantic](https://pydantic-docs.helpmanual.io/)を使う。

### ふつうに実行

JSONを渡すか`file://`付きでファイル名を渡す

```console
$ python 02pydantic-with-schema.py --config='{"main": {"db": "sqlite://:memory:"}}'
Namespace(config=Config(main=MainConfig(db='sqlite://:memory:'), thirdparty=None), show_schema=False)

$ python 02pydantic-with-schema.py --config=file://config.json
Namespace(config=Config(main=MainConfig(db='sqlite://:memory'), thirdparty=ThirdpartyConfig(xxx=XXXConfig(token='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'), zzz=ZZZConfig(clientId='xxxxxxxxxx', clientSecret='xxxxxxxxxxxxxxxxxxxx'))), show_schema=False)
```

### 設定がおかしいとき

テキトーにエラーが出る。

```console
# validation error
$ python 02pydantic-with-schema.py --config=file://config-ng.json
usage: 02pydantic-with-schema.py [-h] [--config CONFIG] [--show-schema]
02pydantic-with-schema.py: error: argument --config: 1 validation error for Config
main
  field required (type=value_error.missing)

# file not found
python 02pydantic-with-schema.py --config=file://missing.json
usage: 02pydantic-with-schema.py [-h] [--config CONFIG] [--show-schema]
02pydantic-with-schema.py: error: argument --config: [Errno 2] No such file or directory: 'missing.json'
```

## appendix

schemaが見えたほうが便利？

```console
$ python 02pydantic-with-schema.py --show-schema
{
  "definitions": {
    "MainConfig": {
      "title": "MainConfig",
      "type": "object",
      "properties": {
        "db": {
          "title": "Db",
          "type": "string"
        }
      },
      "required": [
        "db"
      ]
    },
    "XXXConfig": {
      "title": "XXXConfig",
      "type": "object",
      "properties": {
        "token": {
          "title": "Token",
          "type": "string"
        }
      },
      "required": [
        "token"
      ]
    },
    "ZZZConfig": {
      "title": "ZZZConfig",
      "type": "object",
      "properties": {
        "clientId": {
          "title": "Clientid",
          "type": "string"
        },
        "clientSecret": {
          "title": "Clientsecret",
          "type": "string"
        }
      },
      "required": [
        "clientId",
        "clientSecret"
      ]
    },
    "ThirdpartyConfig": {
      "title": "ThirdpartyConfig",
      "type": "object",
      "properties": {
        "xxx": {
          "$ref": "#/definitions/XXXConfig"
        },
        "zzz": {
          "$ref": "#/definitions/ZZZConfig"
        }
      },
      "required": [
        "xxx",
        "zzz"
      ]
    },
    "Config": {
      "title": "Config",
      "type": "object",
      "properties": {
        "main": {
          "$ref": "#/definitions/MainConfig"
        },
        "thirdparty": {
          "$ref": "#/definitions/ThirdpartyConfig"
        }
      },
      "required": [
        "main"
      ]
    }
  }
}
```
