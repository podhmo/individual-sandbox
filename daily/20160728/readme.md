# djangoでcacheを部分的にまとめて消す方法(backend=redis)

以下で全部は消せるが。部分的にまとめて消す事ができない。
(もちろん個別に消すことは出来る)


```
cache.clear()
```

諦めてbackend specificな方法を使って消す。

## redis

KEYSはglobパターン見てくれるのでpipeで渡す雑な方法(atomicではない)。

```
$ time redis-cli -h localhost -p 6379 KEYS "*<keyname>*" | xargs redis-cli -h localhost -p 6379 DEL
```

# django-silkのprofilerをもう少しまじめに調査したい場合

django-silkはprofilerの実行結果を文字列としてdbに保存するだけなので、profileの結果を他のアプリで使えない。

以下の様な修正をして無理矢理 `/tmp/silk` 以下に出力させる

silk/collector.py

```python

    def finalise(self):
        if hasattr(self, 'pythonprofiler'):
            s = StringIO()
            ps = pstats.Stats(self.pythonprofiler, stream=s).sort_stats('cumulative')
            ps.print_stats()
            profile_text = s.getvalue()
            profile_text = "\n".join(
                profile_text.split("\n")[0:256])  # don't record too much because it can overflow the field storage size
            self.request.pyprofile = profile_text

+             ### hmm
+             import os.path
+             dump_name = "silk-profile-request-{}-{}.prof".format(self.request.id, self.request.path.replace("/", "_"))
+             if not os.path.exists("/tmp/silk"):
+                 os.makedirs("/tmp/silk")
+             ps.dump_stats("/tmp/silk/{}".format(dump_name))
+             ###
        for _, query in self.queries.items():
            query_model = models.SQLQuery.objects.create(**query)
```

あとは `snakeviz` などで見る。localの環境以外でやるのはおすすめしない。
