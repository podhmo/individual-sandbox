## python jupyter nbconvertの仕組み

使い方の例

```
$ jupyter-nbconvert --execute --to html Untitle.ipynb
```

### `--to` で変換先を指定できる。

ここで呼ばれるものは、nbconvert.exportersの中で定義されている。(例えば、ipynbはnbconvert.exporters.notebook)
多くのexportersはTemplateExporterを継承している。これは実行結果のnotebook objectをjinja2 templateを使ってrenderingするもの。

ちなみに`--to`の分岐は、`nbconvert.exporters.get_exporter()`で処理されている。この分岐には[entrypoints](https://github.com/takluyver/entrypoints)が使われている。内部的にはsys.path以下を愚直に走査していく感じなので遅い(柔軟ではある)。

### `--execute` で実行結果を出力に埋め込める。

この `--execute` はどこで解釈されるのかというと、nbconvert.preprocessorsの中で定義されているnbconvert.preprocessors.execute.ExecutePreprocessor。そう。preprocessorとして定義されている。ちなみにdefaultで有効なpreprocessorはこういう感じ。

```python
[
    'nbconvert.preprocessors.RegexRemovePreprocessor',
    'nbconvert.preprocessors.ClearOutputPreprocessor',
    'nbconvert.preprocessors.ExecutePreprocessor',
    'nbconvert.preprocessors.coalesce_streams',
    'nbconvert.preprocessors.SVG2PDFPreprocessor',
    'nbconvert.preprocessors.CSSHTMLHeaderPreprocessor',
    'nbconvert.preprocessors.LatexPreprocessor',
    'nbconvert.preprocessors.HighlightMagicsPreprocessor',
    'nbconvert.preprocessors.ExtractOutputPreprocessor',
]
```

preprocessorは各自がpreprocessというメソッドを持っている。この中で[jupyter_client](https://github.com/jupyter/jupyter_client) を利用してkernelを立ち上げ通信している。ただ実行結果を埋め込むまでが長い(遅い)。

executenbという関数も用意されているので最短は以下の様な形でnotebookに結果を埋め込める。

```python
import nbformat
from nbconvert.preprocessors.execute import executenb

def render(filename):
    with open(filename) as rf:
        nb = nbformat.read(rf, as_version=4)
    return executenb(nb)
```
