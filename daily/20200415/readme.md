## python go pythonでgoのコードを生成してみる

今回は生成することを目的にするのではなく、なるべく自然に表現することを目的にしてみる。

- ifなどでformatが使えたほうがやっぱり便利そう
- typeを指定する方法が存在しているとやっぱり嬉しい
- Symbolがあると、急に`return_()`が嬉しくなる
- importの対応を考えたい

### 追記

もう少しASTの変換も込みで考えてみた。

- 変数定義を真面目に行うのがだるい
- 代入の意味が複数あるときに困る