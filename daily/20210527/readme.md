## sse 再開を考えてみる

sseはreconnectが自動なのが嬉しいのだけれど

- 切断されてreconnect
- 新規に接続してconnect (reload)

の場合がある。たぶん判断する術がない。

例えば、sseで実行ログを出力しているときに、過去分の情報を取って来たいとなったらどうするか？
これは、答えは別途状態を取得するAPIを用意するんだと思う。

- unmaterialized state
- materialized state

みたいな状態があり、ふわふわした状態のunmaterialized stateがsseとして出力と言う感じ。
過去分の情報は既に固まった履歴なのでmaterialized state。必要ならclientがリクエストする。

参考

- https://github.com/singingwolfboy/flask-sse
