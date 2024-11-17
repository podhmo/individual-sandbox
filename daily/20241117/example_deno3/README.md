# esm.shにnpm:の依存を逃がしたcomponent.mjsの作成

このgistのtools2.tsを使ってcomponent.mjsを作成した。

- [denoで読み込めるファイルをhtmlで読み込めるesmに変換したい](https://gist.github.com/podhmo/be654d37700e1c3c6590769540f38abc)

## 使い方

```console
$ make server
```

http://localhost:8080

(python3 -m http.serverは手癖で使っているだけ)

## 補足

以下のようにpreactのrenderをhtml側で利用するときにバージョンを合わせて調整しないとダメになるのでcomponent.mjsの中でexportしている。
app.mjsみたいな名前のほうが適切かも？

```html
    <script type="module">
        import { render } from "https://esm.sh/preact@10.24.3" // ここを揃える必要があるのが難しいかも？
        import { Hello } from './dst_main5.mjs';
    
        render(Hello({name: "World"}), document.querySelector('#app'));
    </script>
```
