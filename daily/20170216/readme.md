# python pyramid rest api

この辺知らなかった

- https://github.com/ramses-tech/nefertari
- https://github.com/ramses-tech/ramses

昔からある

- https://github.com/Cornices/cornice

知りたかったのはHTTPExceptionのresponseの形式だけだった。

## 雑に開発するだけで良いというなら

pyramidのhttpexceptions.pyを除いてみたらprepare()のところでHTTP_ACCEPT見てる。これがapplication/jsonだった場合にはjsonになるっぽい。
stack traceの有無的なことを確認したかったり。

ACCEPTヘッダー渡す。
ただし、internal server errorのときには自動的にtext/plainになってしまう？

## responseの仕様

と言うかこの辺見ておけば良さそうな感がある。

- [HTTP APIの詳細なエラー情報をレスポンスに持たせるための仕様](https://www.eisbahn.jp/yoichiro/2017/01/rfc_7807.html)
- [WebAPIでエラーをどう表現すべき？15のサービスを調査してみた - Qiita](http://qiita.com/suin/items/f7ac4de914e9f3f35884)


