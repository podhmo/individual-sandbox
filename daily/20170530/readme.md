# python logging

[./example_logging]

http://qiita.com/usatosi/items/25e357a3fad6376ebcaf

もう少し綺麗にかけそうな気がした。


どれが良いのか答えが出なかった

- 00original -- 元のqiitaの記事の実装(全てのオブジェクトを真面目にloggingのroleを使って実装している)
- 01newhandler -- 1つのファイルを扱うHandlerではなく、複数のファイルを扱うHandlerということにして実装
- 02primitives -- 00originalと同様の理屈だが、Filterオブジェクトを使わずwrapperで包むバージョン

嫌なポイント

- 特定のログレベルのものだけを出したいという要求
- (ところでinfoが取り扱われていないけれど良いの。。)

# jq jq覚えると良い

[./example_jq]

- jq
- `jq -S`
- `jq -S "sort_by(<expr>)"`
- `jq -S --slurp "sort_by(<expr>)"`
