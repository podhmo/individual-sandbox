# httpbin がどれくらいの機能を提供しているのか見てみる。

`/spec.json` でJSONが手に入るっぽい。

```console
$ http -b https://httpbin.org/spec.json
```

けっこうきれいにかかれているのでこういうことができそう。

```console
$ http -b https://httpbin.org/spec.json |  jqfpy -i yaml '[[f""" "{sd.get("tags", [""])[0]}", {method.upper()},"{path}","{sd.get("summary")}" """.strip() for method, sd in v.items()] for path,v in get("/paths").items()]' /tmp/y --squash --squash -r | dictknife cat -i csv -o md
```

