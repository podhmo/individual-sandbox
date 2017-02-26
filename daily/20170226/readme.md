# yaml pyyamlの出力時のreferenceを無効にする

```python
import yaml


class IgnoreReferenceDumper(yaml.Dumper):
    def ignore_aliases(self, data):
        return True

yaml.dump(d, Dumper=IgnoreReferenceDumper)
```

# swagger

swagger自身のjsonschemaが普通にあった。

- https://github.com/OAI/OpenAPI-Specification/blob/master/schemas/v2.0/schema.json

examples

- https://github.com/OAI/OpenAPI-Specification/tree/master/examples

# dictknife

- json pointerをサポート
- load,dumpが面倒くさい問題

commandsの辺り整理したい。

そういえばexampleを結合できるようになれば良いんじゃない？

## dictknifeでのbundle

そもそもbundleとは。。

- $refにまとめない形はとても簡単
- 難しいのは共通のrefを使う形(名前が衝突した時のことを考えると辛い。)


