#[python][jqfpy][memo]今年作ったgithubのrepositoryを調べる

もしかしたら今年も作ったrepositoryを振り返えったりするかもしれないので。そのメモ。
今年作ったパブリックなrepositoryの一覧を手に入れる方法について。

## setup

残念ながら依存関係があるので以下のインストールが必要。

```
pip instlal httpie jqfpy
```


## 今年作ったrepository一覧

### code

makefileにした。`ME`に誰かテキトウな名前を入れるとその人が今年作ったrepositoryがわかる。

```make
# setup:
# pip install httpie jqfpy

ME ?= podhmo

repos.json:
	http https://api.github.com/users/${ME}/repos page==1 per_page==100 | jq . | tee $@
repos2.json:
	http https://api.github.com/users/${ME}/repos page==2 per_page==100 | jq . | tee $@

parse: repos.json repos2.json
	jqfpy -c --squash 'from datetime import datetime as d; y = d(2017,1,1); [h.pick("name","created_at",d=x) for x in get() if not x["fork"] and d.strptime(x["created_at"], "%Y-%m-%dT%H:%M:%SZ") > y ]' repos*.json | jqfpy -c --squash --slurp 'sorted(get(), key=lambda x: x["created_at"])'
```

実はtimezoneの扱い真面目にやっていないとか。pagination真面目に対応していないとかがあるのであまりきれいな感じじゃないけれど。

### 今年作ったrepository

今年はこんな感じ。

```shell
$ ME=podhmo make -B parse

{"name": "pyramid-swagger-router", "created_at": "2017-01-01T12:18:53Z"}
{"name": "toybox", "created_at": "2017-02-15T07:42:48Z"}
{"name": "zenmai", "created_at": "2017-04-15T08:41:58Z"}
{"name": "nejimaki", "created_at": "2017-05-01T02:58:26Z"}
{"name": "json2swagger", "created_at": "2017-05-02T16:23:55Z"}
{"name": "yayapf", "created_at": "2017-05-04T05:00:09Z"}
{"name": "goaway", "created_at": "2017-05-05T05:20:58Z"}
{"name": "kamidana", "created_at": "2017-05-13T07:28:21Z"}
{"name": "monokaki", "created_at": "2017-05-21T06:29:48Z"}
{"name": "hatena", "created_at": "2017-05-28T03:43:25Z"}
{"name": "moduleknife", "created_at": "2017-06-07T09:30:24Z"}
{"name": "pokeapi-sandbox", "created_at": "2017-06-30T11:21:27Z"}
{"name": "utatane", "created_at": "2017-07-21T06:44:58Z"}
{"name": "yieldfixture", "created_at": "2017-07-22T09:29:49Z"}
{"name": "nbreversible", "created_at": "2017-07-28T13:22:17Z"}
{"name": "pickuppath", "created_at": "2017-08-02T12:58:47Z"}
{"name": "jqfpy", "created_at": "2017-08-26T06:20:44Z"}
{"name": "familiar", "created_at": "2017-09-12T12:24:34Z"}
{"name": "fib", "created_at": "2017-09-19T22:56:05Z"}
{"name": "cellar", "created_at": "2017-10-01T09:14:25Z"}
{"name": "testmarker", "created_at": "2017-10-07T05:23:58Z"}
{"name": "reqtrace", "created_at": "2017-12-15T14:28:35Z"}
```

結構ゴミみたいなrepositoryもつくってるし。あんまり今年は作っていないかも。
