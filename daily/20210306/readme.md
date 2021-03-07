## go gokitが便利な気がする

gokit/color

## go

- handlerのテストをいい感じに
- jsendを使った例

## go tenuki

久しぶりに触っていたら粗が気になった

- http.TestServerを気にせず扱いたい
- client()で状態を持っているのが気持ち悪い
- Capture()という状態を持つ関数が気持ち悪い
- 複数のRequestを束ねたcaptureもあり得る？

### Capture()を消す

../20200928/example_go/00api-key/main_test.go などで使われているらしい。
facadeを毎回作れば良いのでは？

### request/responseを保存したい

- writefile的なオプションを付ける
- file名にテスト名をくっつけたい

### traceの表示を見やすくしたい

- JSONで良いのでは？
- harと比べてみる

実装をどうしよう

- structを真面目に定義
- reflectで雑に集める

harと比べてみた結果

- cookieとかない
- serverのIPAddressとかない

### 追記

冷静になって考えてみると、以下の方が嬉しいのでは？

- openAPIの構造に合わせる
- それをparseしてrequestを作れる

色々と仕様用のものなのでうまくいかないけれど。。

### 追記

https://github.com/vvakame/go-harlog
