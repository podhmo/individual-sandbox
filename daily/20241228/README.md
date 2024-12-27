# esm.shへのrequestをcacheしたい

昨日の続きで自分がフロントエンドでの試行錯誤を手軽にやれるような環境を作っていきたい。

- 昨日のもの https://gist.github.com/podhmo/5285897f6d5b98b9dad76183cdf4f832

とりあえず今回はテキトーに自分自身へrequestしてキャッシュしたresponseを返すような形にしたい。

## 00 

とりあえず昨日の続きから始める。シンプルにcacheなどをせずrequestをそのままesm.shに渡すサンプルを作る。

