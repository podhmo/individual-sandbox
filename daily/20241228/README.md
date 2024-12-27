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
いや、302のリダイレクトとか関係なくesm.shで返されるpath自体が自分自身のpathなのでproxyのendpointに渡らない。
とりあえず `GET /*` を全部esm.shにforwardすることにする（しかしこれは危険というかesm.shに雑に投げてしまう感じになって良くないなー。）

### requestを眺める

`/stable/*`, `/v135/*` などを見に行く。これを変更するには返してくるコード自体を書き換える必要があるので厳しそう。。

```console
$ deno serve -A --port 8080 01app.ts
deno serve: Listening on http://0.0.0.0:8080/
proxy request : https://esm.sh/stable/react@18
proxy response: 200, duration: 626.8521359999995
[DEBUG] setup resolve jsr: -> /stable/jsr/
[DEBUG] setup resolve npm: -> /stable/
[DEBUG] resolve npm:react@18
[DEBUG] resolve npm:react-dom@18/client
[DEBUG] resolve npm:react@18/jsx-runtime
[DEBUG] setup resolve jsr: -> /stable/jsr/
[DEBUG] setup resolve npm: -> /stable/
[DEBUG] resolve npm:react@18
[DEBUG] resolve npm:react-dom@18/client
[DEBUG] resolve npm:react@18/jsx-runtime
proxy request : https://esm.sh/stable/react@18
proxy request : https://esm.sh/stable/react-dom@18/client
proxy request : https://esm.sh/stable/react@18/jsx-runtime
proxy response: 200, duration: 645.4978080000001
proxy request : https://esm.sh/stable/react@18.0.0/es2022/react.mjs
proxy response: 200, duration: 24.092055999999502
proxy request : https://esm.sh/v135/node_process.js
proxy response: 200, duration: 25.98497900000075
proxy response: 200, duration: 745.8616320000001
proxy request : https://esm.sh/v135/scheduler@0.25.0/es2022/scheduler.mjs
proxy request : https://esm.sh/v135/react-dom@18.0.0/es2022/react-dom.mjs
proxy response: 200, duration: 21.043251999999484
proxy request : https://esm.sh/v135/react-dom@18.0.0/es2022/client.js
proxy response: 200, duration: 59.307482000000164
proxy response: 200, duration: 596.0807650000006
proxy request : https://esm.sh/v135/node_events.js
proxy request : https://esm.sh/stable/react@18.0.0/es2022/jsx-runtime.js
proxy response: 200, duration: 59.78629000000001
proxy response: 200, duration: 31.458613000000696
proxy response: 200, duration: 35.009971999999834
proxy request : https://esm.sh/stable/react@18.0.0/es2022/react.mjs.map
proxy request : https://esm.sh/v135/scheduler@0.25.0/es2022/scheduler.mjs.map
proxy request : https://esm.sh/v135/react-dom@18.0.0/es2022/react-dom.mjs.map
proxy response: 200, duration: 39.90736199999992
proxy response: 200, duration: 53.80151300000034
proxy response: 200, duration: 56.36429599999974
proxy request : https://esm.sh/v135/react-dom@18.0.0/es2022/client.js.map
proxy request : https://esm.sh/stable/react@18.0.0/es2022/jsx-runtime.js.map
proxy response: 200, duration: 25.828926000000138
proxy response: 200, duration: 257.2474149999998
```

## 02 requestをcacheしたい

- cache control headerを真面目に書く (esm.sh自体が付けてそう)
- ファイル配信自体をキャッシュしてローカルから行う

後者とかもnginxがやってくれそうな気もしなくもないが雑にcacheしてみることにする。

- https://github.com/denosaurs/cache

### 謎のエラー

変な秘孔をついてしまったかも。。 `https://esm.sh/stable/react@18` にリクエストしたときだけlstatでエラーを起こすっぽい。

```
filepath:  /home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j
error: Uncaught (in promise) TypeError: file name contained an unexpected NUL byte: lstat '/home/po/.cache/deno/podhmo-glue/https/esm.sh/�g��E=m�Y:M���t ���-�Ƀ�c���j'
    await Deno.lstat(filePath);
```

以下のときに起きる？

- "jsr:@denosaurs/cache@0.2.15"
- "https://esm.sh/stable/react@18"
- deno 2.1.4
- (WSLのUbuntuの環境)

（ https://gist.github.com/podhmo/7e485256563c8d0d0539d916b34f47d8#file-cache-example-ts で再現する ）

とりあえず `npm:react@18` ではなく `npm:react@19` にして逃げた。

これを組み込みにしてあげたらふつうに動くのだけど、不要なリクエストを投げる可能性があるのはどうなんだ。。という気持ちになったりもする。

あとpurge用のオプションを用意したりしたほうが良さそう。まぁそれはそれとして早い。

#### もう少し追ってみた

- 起きる jsr:@denosaurs/cache@0.2.15
- 起きる jsr:@denosaurs/cache@0.2.14
- 起きない https://deno.land/x/cache@0.2.13/mod.ts

#### 更にもう少し追ってみる。

以下の様なコードでヌル文字が生成されるということらしい。


```ts
async function hash(url: URL): Promise<string> {
    const formatted = `${url.pathname}${url.search ? "?" + url.search : ""}`;
    const encoder = new TextEncoder();
    const data = encoder.encode(formatted);
    const hash = await crypto.subtle.digest("SHA-256", data);
    return new TextDecoder().decode(hash);
}

const url = "https://esm.sh/stable/react@18";
const calculatedPath = await hash(new URL(url));
console.log("calculated path", calculatedPath); // true
console.log(await Deno.lstat(calculatedPath)); // TypeError: file name contained an unexpected NUL byte:
```