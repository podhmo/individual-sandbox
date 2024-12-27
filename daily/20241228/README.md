# esm.shへのrequestをcacheしたい

昨日の続きで自分がフロントエンドでの試行錯誤を手軽にやれるような環境を作っていきたい。

- 昨日のもの https://gist.github.com/podhmo/5285897f6d5b98b9dad76183cdf4f832

とりあえず今回はテキトーに自分自身へrequestしてキャッシュしたresponseを返すような形にしたい。

## 00 esm.shでreactのcounterを作る

とりあえず昨日の続きから始める。シンプルにcacheなどをせずrequestをそのままesm.shに渡すサンプルを作る。

## 01 proxy requestを追加

自分指針の `GET /esm-sh/*` にリクエストしてみる。ここでは単にそのままproxyを返す。
雑にChatGPTに聴いてみたコードを後の参考にするかもしれない。

- https://chatgpt.com/share/676ed6fc-6fc0-8001-b490-e8162551edf1

考えてみると、302とかでリダイレクトしたときにも対応しないとだめなのか。