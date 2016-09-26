# python yaml

PyYamlで順序つきでload,dump

```python
import yaml


def represent_odict(dumper, instance):
    return dumper.represent_mapping('tag:yaml.org,2002:map', instance.items())


def construct_odict(loader, node):
    return OrderedDict(loader.construct_pairs(node))


def setup():
    yaml.add_constructor(yaml.resolver.BaseResolver.DEFAULT_MAPPING_TAG, construct_odict)
    yaml.add_constructor(u'tag:yaml.org,2002:map', construct_odict)
    yaml.add_representer(OrderedDict, represent_odict)
```

# wip python process run loop

特定のprocessがずっと起動していたほうが多重実行を考えずに住むので楽。asyncioで雑に実装できないかどうか。

