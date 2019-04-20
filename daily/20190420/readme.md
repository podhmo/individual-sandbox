## webapi client library

https://speakerdeck.com/player/ba29199aeb3344cd835bad09db424663?title=false&skipResize=true

## elixir phoenixのliveview

必要なもの

- mix

https://qiita.com/kikuyuta/items/c5b0788ad5d09c210c0a
https://qiita.com/piacere_ex/items/3f8ee18c9443d63955bf
https://github.com/chrismccord/phoenix_live_view_example
https://dockyard.com/blog/2018/12/12/phoenix-liveview-interactive-real-time-apps-no-need-to-write-javascript

### 重要なこと

- 認証に使うsalt
- live viewファイルのlive reload
- node modulesは必要なのか...
- render(), mount()という最初のhook point
- live viewになったときの全体の状態はどこに持つ？ -> handlerでupdate()という関数越しに更新
- errorが出たときのメッセージング(phx-error, phx-disconnected)
- template engine的な用語を覚えないとダメ(LiveEEX)。このdiffの実装がきになる。morphdomそのまま使っているわけではなさそう

```
<button phx-click="action"><%= @val %></button>
```


## python .envを読み込む

https://pypi.org/project/python-dotenv/


## make conflictし辛いmakefile

```make
foo: bar boo
	do something
```

を

```make
foo:\
 bar\
 boo
	do something
```

にする
