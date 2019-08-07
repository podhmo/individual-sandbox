## ioknife too SIGHOPで良い感じに終了する

- https://pod.hatenablog.com/entry/2019/08/07/163409

これの対応。

### 追記

SIGHUPよりSIGTERMの方が良いかも。

- https://ascii.jp/elem/000/001/467/1467705/

## shosai haten 500

まじめに追っていないけれどだるいな。

```console
$ shosai hatena clone https://pod.hatenablog.com/entry/2019/08/07/163409
INFO:shosai.hatena.configuration:read: $HOME/.config/shosai/config.json
INFO:shosai.hatena.resources:GET https://blog.hatena.ne.jp/podhmo/pod.hatenablog.com/atom/entry, params={}
INFO:shosai.hatena.resources:status=500, https://blog.hatena.ne.jp/podhmo/pod.hatenablog.com/atom/entry
WARNING:shosai.commands.shosai:500 Server Error: Internal Server Error for url: https://blog.hatena.ne.jp/podhmo/pod.hatenablog.com/atom/entry -- 'Internal authorization error'
```

