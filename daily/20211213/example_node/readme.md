# node

## vscodeで閉じタグを同時に入力してくれる機能なんだっけ？

- [コードを書くのが楽になる！知っておくと便利なVS Codeの機能・設定のまとめ | コリス](https://coliss.com/articles/build-websites/operation/work/vs-code-dont-need-extensions.html)
- [Emmet — the essential toolkit for web-developers](https://emmet.io/)

とりあえずEmmet wrapping with abbrebiationでいけるみたい？

- C-c C-e でwrappingするか
- C-c e でupdate

## npm何で入れよう？

とりあえずテキトーにWSL2上のubuntuで `apt install npm` して入れたけど良くないかも？ `npm install -g npx` もしている。

そもそもnode.jsのバージョンが古いようだ。

https://github.com/tj/n

```console
curl -L https://raw.githubusercontent.com/tj/n/master/bin/n -o /tmp/n
N_PREFIX=~/.local/n bash -x /tmp/n lts
```

### 何か失敗しているかも？


```console
$ npx create-remix@latest
Cannot find module 'is-fullwidth-code-point'

$ npm serach is-fullwidth-code-point
NAME                      | DESCRIPTION          | AUTHOR          | DATE       | VERSION  | KEYWORDS                                                                                     
is-fullwidth-code-point   | Check if the…        | =sindresorhus   | 2021-04-16 | 4.0.0    | fullwidth full-width full width unicode character string codepoint code point is detect check
```

npmでのinstallは可能。npxでは無理という謎の状態。npxで使われるnpmがなにか違うのものになっている？

なるほど、guestのほうではなくhostの方のコマンドが使われていた (`npm install -g npx` 忘れ)


## chakra ui をいじってみる

remixと一緒にしてみるか

- https://chakra-ui.com/guides/getting-started/remix-guide

```console
npm i @chakra-ui/react @emotion/react@^11 @emotion/styled@^11 framer-motion@^4
```

react iconsなどの存在がわかっていない。


```tsx
import { ChakraProvider } from '@chakra-ui/react'

export default function App(){
    return (
      <Document>
          <ChakraProvider>
              <Outlet />
          </ChakraProvider>
      </Document>
    );
}
```

あんまりわかっていないけれど、Appをproviderで包む感じ？
例外発生時などの対応も書いておきたいらしい。

```tsx
import { ChakraProvider } from '@chakra-ui/react'

export function ErrorBoundary({ error }: { error: Error }) {
    return (
      <Document title="Error!">
          <ChakraProvider>
              <Box>
                  <Heading as="h1">There was an error</Heading>
              </Box>
          </ChakraProvider>
      </Document>
    );
}

export function CatchBoundary() {
    let caught = useCatch();

    return (
      <Document title={`${caught.status} ${caught.statusText}`}>
          <ChakraProvider>
              <Box>
                  <Heading as="h1">
                    {caught.status} {caught.statusText}
                  </Heading>
              </Box>
          </ChakraProvider>
      </Document>
    );
}
```

providerが持つpropsは resetCSS, theme, colorModeManager, portalZIndex
themeの変更はこの辺で。

https://chakra-ui.com/guides/getting-started/remix-guide#3-optional-setup

### typescript

そういえば、typescript用の初期設定がわからない。

- [Vite で最速 React & TypeScript](https://zenn.dev/sprout2000/articles/98145cf2a807b1)
- [Getting Started | Vite Next Generation Frontend Tooling vitejs.dev vitejs.dev](https://vitejs.dev/guide/)

どうやら、viteを使えば楽なようだ。create-react-app自体dev server周りが重たいらしいしunmaintainedっぽい。

- [We need regualr CRA maintainer · Issue #11180 · facebook/create-react-app](https://github.com/facebook/create-react-app/issues/11180)


## remix

https://remix.run/docs/en/v1/tutorials/blog

```console
npx create-remix@latest
cd <project>
npm run dev
```

この時点でtypescriptを選べるんだな。そしてexpressを選ぶのかremix serverを選ぶのかよくわからない。
Remix App Serverというものは何者？

これでとりあえずdev serverが立ち上がる感じなんだろうか？とりあえず、portが3000で動いた。

### your first root

app/root.tsxを作る

```tsx
<li>
  <Link to="/posts">Posts</Link>
</li>
```

app/routes/post/index.tsxを作る

```
mkdir -p app/routes/post/
touch app/routes/post/index.tsx
```

index.tsx

```tsx
export default function Posts() {
    return (
        <div>
            <h1>Posts</h1>
        </div>
    );
}
```

これが上手く反応しない。チュートリアルの通りにminimumに整形してから使うか。
そして、`application/javascript; charset=UTF-8`のresponseが帰る感じなのか。

```javascript
import {
  React,
  init_react
} from "/build/_shared/chunk-E7VMOUYL.js";

// browser-route-module:/home/podhmo/ghq/github.com/podhmo/individual-sandbox/daily/20211213/example_node/my-remix-app/app/routes/posts/index.tsx?browser
init_react();

// app/routes/posts/index.tsx
init_react();
function Posts() {
  return /* @__PURE__ */ React.createElement("div", null, /* @__PURE__ */ React.createElement("h1", null, "Posts..."));
}
export {
  Posts as default
};
//# sourceMappingURL=/build/routes/posts/index-EA5BCKWU.js.map
```

なるほど、layoutで貫通させて表示させる方法がわからない(defaultはlayoutでwrapされている)
あ、ただのreactのSFCか。

たぶん以下がわかれば良い

- Layout -> `{children}`
- Outlet

[React Router v6 | Remix](https://remix.run/blog/react-router-v6)
チューニングの方法とかが頭の中にないのでnext.jsのほうが楽なのでは？

とりあえず、chakura uiのcomponentを表示させる事はできたが、remixを利用する意味はなさそう。

