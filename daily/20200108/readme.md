## どうしよう？

- handof catsのlogを追加
- 


## python google

違うんだ。。

- https://github.com/googleapis/google-api-python-client
- https://github.com/googleapis/google-cloud-python#google-cloud-python-client

## oauth関係を手軽に確認できるようななにか

### なんかbad request

token infoみたいなendpointなかったっけ？ (いろいろ調べた結果↓っぽい）

- https://developers.google.com/identity/sign-in/web/backend-auth#calling-the-tokeninfo-endpoint
- https://oauth2.googleapis.com/tokeninfo?id_token=XYZ123

- 仕様があったりする？

  - https://tools.ietf.org/html/rfc7662
  - https://qiita.com/minamijoyo/items/bd76ce780e4551d7c951
  - https://qiita.com/shin1ogawa/items/49a076f62e5f17f18fe5
  - https://developers.google.com/identity/protocols/OpenIDConnect#validatinganidtoken

この情報はどこにあるんだ。。 https://www.googleapis.com/oauth2/v3/tokeninfo

RFC7662と別物では？

- https://github.com/jazzband/django-oauth-toolkit/pull/477/files
- https://ritou.hatenablog.com/entry/20151021/1445438454
- https://github.com/zalando/connexion/issues/475
- https://oauth.com/oauth2-servers/token-introspection-endpoint/

hmm

- https://github.com/googleapis/google-api-go-client/blob/6a6a093af5a2aa81aa33ee0bdaa85e50c4be7371/oauth2/v2/oauth2-api.json#L39-L61

### 構成

- https://github.com/ory/hydra

## やっぱりhandofcatsのsubcommandの対応がほしい

## dictknife spreadsheetの連携を書き直したい

- oauth2clientはdeprecatedなので
- multi profileにしたい
- list,info,revoke

## dictknife envを変えたいかも？
