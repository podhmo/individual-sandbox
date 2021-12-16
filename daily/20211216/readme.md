## vite typescript

- https://ics.media/entry/210708/
- https://dev.classmethod.jp/articles/vite-react-preact-typescript-tailwindcss/

### vite react-ts

初期化はこんな感じ。

```console
npm init vite@latest my-app
```

選択肢はvanilla,vue,react,preact,lit,svelteがあるらしい。
reactを選ぶとその後variantとしてreact,react-tsが存在する

```console
$ tree .
.
└── my-app
    ├── index.html
    ├── package.json
    ├── src
    │   ├── App.css
    │   ├── App.tsx
    │   ├── favicon.svg
    │   ├── index.css
    │   ├── logo.svg
    │   ├── main.tsx
    │   └── vite-env.d.ts
    ├── tsconfig.json
    └── vite.config.ts

2 directories, 11 files
```

npm installして、npm run devしろとのこと。

### react

`const [count, setCount] = useState(0)` のあとの `<button type="button" onClick={() => setCount((count) => count + 1)}>` のどうするか？の話忘れてしまうな。。関数を渡さないとだめだったのだっけ？もともとクラスであれこれがあって、thisで悩まないようにするためにarrow functionで定義するという感じだった。FCでも同様だったような記憶。関数にしなくても動きはするもののstate managerに送られず即時実行されちゃうので関数で包んだほうが良いという感じだったと思う。functionで書く理由も特になくなったのだっけ？(constで代入しておけばトレースバックで見る事ができる)

HMRがまともに動いて体験は良い（hello world）的なアプリなせいもあるけれど。

### svelte

svelteもtsが存在している。document.getElementByIdで要素を指定するのは変わらないなー。
自分が知りたいのは動的な機能の付け方ではなくレイアウトの技法かもしれない。それも手軽でいい感じにという形で。

## typescript

もっと小さなtypescriptだけの世界を試そうと思ったときにはどうする？
denoを使えば良いのでは？

### deno

https://deno.land/

```console
$ sudo apt install unzip
$ ( export DENO_INSTALL=~/.local/deno; curl -fsSL https://deno.land/x/install/install.sh | sh )
```
コードの実行方法

```console
$ deno run <file>
```

- そういえば、メソッドは大文字で始まるものとは限らないんだな。
- `Deno.<something>` がsysみたいなものか。
- go doc的なものが知りたい。

https://doc.deno.land/