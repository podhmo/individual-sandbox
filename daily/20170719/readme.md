## elasticsearch pluginのinstall

```bash
$ elasticsearch-plugin install --batch analysis-icu
```

- https://www.elastic.co/guide/en/elasticsearch/plugins/current/installation.html

## python 全角半角など色々正規化

NFKCなどで正規化してしまえば良さそう

```python
import unicodedata

unicodedata.normalize("NFKC", "１1")
```
