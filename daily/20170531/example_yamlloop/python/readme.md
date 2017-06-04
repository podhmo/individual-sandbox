# jqむずかしい

apps.json

```json
{
  "apps": {
    "foo": {
      "use": true
    },
    "bar": {
      "use": true
    },
    "boo": {
      "use": true
    },
    "bee": {
      "use": false
    }
  }
}
```

このJSONからuseがtrueのものをsortして取り出したいだけなのだけれど。jqでうまく書くことができない。
pythonですら(なんとか)ワンライナーで書けるというのに。

```console
$ cat apps.json | python -c 'print("\n".join(sorted(k for k, v in __import__("json").load(__import__("sys").stdin)["apps"].items() if v["use"])))'
bar
boo
foo
```
