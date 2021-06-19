## data loader

pythonでdata loaderみたいなことをやってみる。

- 肝は時間的にgroupingする点
- asyncio.Queueをうまく使えば良い？うまくfutureを返すようにしなくてはならない。

どういうexampleが良いのだろう？単純にはこんなやつ。

- team = {A, B}
- User = {x,y,z,i}, {x,j,k}

getTeam(), getUser()というような形。でもこれだけだと結構シンプルにbulkで取ってこれちゃう気もするが。

### python

- 意外とめんどくさいね。。

  - sentinelで待つ部分をdefaultdict(list)にしないでハマってしまった

- 実行をまとめるのは自分でやらないとダメ
- 発行するqueryはまとめてくれる

### 追記

data loaderがカッコ良いかつ怖ろしいところは、requestを超えてqueryをまとめてくれる余地がある所
