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

## mongodb mondogbのsparse index

```
db.createCollection("entries");

db.entries.insert([
  {"_id": ObjectId(), "name": "foo"},
  {"_id": ObjectId(), "name": "bar"},
  {"_id": ObjectId(), "name": "boo"},
]);

// create sparse index
db.entries.createIndex({"markedAt": 1}, {sparse: true});

// markedAt 1 and other items don't have markedAt
db.entries.updateOne({}, {$set: {"markedAt": ISODate()}})
db.entries.find({markedAt: {$exists: true}}).explain("executionStats") // IXSCAN
db.entries.find({markedAt: {$exists: false}}).explain("executionStats") // COLLSCAN

// markedAt 1 and other items markedAt are null
db.entries.updateMany({}, {$set: {"markedAt": null}});
db.entries.updateOne({}, {$set: {"markedAt": ISODate()}})
db.entries.find({markedAt: {$ne: null}}).explain("executionStats") // COLLSCAN


// create dense index
db.entries.dropIndex("markedAt_1");
db.entries.createIndex({"markedAt": 1}); // IXSCAN


// markedAt 1 and other items markedAt are null
db.entries.find({markedAt: {$ne: null}}).explain("executionStats")

// markedAt 1 and other items don't have markedAt
db.entries.updateMany({}, {$unset: {"markedAt": 1}});
db.entries.updateOne({}, {$set: {"markedAt": ISODate()}})
db.entries.find({markedAt: {$exists: false}}).explain("executionStats") // COLLSCAN
```

## mongodb mongodbのexplain

```
db.<collection>.find(q).explain("executionStats")
```

- winningPlanが採用されたもの
- (rejectedPlanが非採用)
- 使われているindexはinpuStageがIXSCANの時のindexName
- (IXSCAN=index scan, COLLSCAN=collection scan)
- totalDocsExaminedはindexで絞り込まれたdocument数
- nReturnedは返されたdocument数

see also

- https://docs.mongodb.com/manual/reference/explain-results/

