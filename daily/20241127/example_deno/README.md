# ちょっとしたjsxを作ってみたい

## main.tsx

とりあえず deno runで実行して結果を得る事ができた。
`@jsx E` とか書いてあげるとそれがjsxFactoryとして利用される。

```console
$ deno run -A main.ts
{ tag: "p", props: null, children: [ "Hello" ] }
<p>Hello</p>
```

問題は型エラーがうざったいところ
> [deno-ts] JSX element implicitly has type 'any' because no interface 'JSX.IntrinsicElements' exists.

ちなみにFragmentには対応していない。

## main2.tsx

雑にnamespaceを追加したらどうにかなった。

```ts
declare global {
    namespace JSX {
        interface IntrinsicElements {
            [elemName: string]: unknown;
        }
    }
}
```

ところで分けてもうまく動くんだろうか？

minijsx.tsに分けた。ただし、globalにぶち込むのは最終手段なきがする。