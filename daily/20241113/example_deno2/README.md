# markdownからjsx経由でhtmlを生成したい

>[!NOTE]
> deno.json無しですすめているけれどなんの縛りプレイなんだろう？

## render0.tsx

とりあえず、HTMLで出力するようにしてみる。全部を１ファイルにまとめておく。deno.json無しでも十分書けるんだろうか？

react-markdownが使えるんだろうか？

https://github.com/remarkjs/react-markdown

> This package is ESM only

テキトーなmarkdownを用意しておく。

## render1.tsx

markdownファイルからHTMLを出力するようにしてみる。これはかなりそのままな気がする。


## render2.tsx

cssなどを追加するのはどうすれば良いんだろう？そのまま追加するだけかも？
以前の結果を使えば良い。

[JSXを弄りたい gist](https://gist.github.com/podhmo/41580702ce448710dcbcc6e06dbec548) 

型情報が手に入っていない。`npm:@types/react`が必要？

## render3.tsx

どうやらtableが変換されていないようだ。あとシンタックスハイライトも効いてない気がする。

どうやらremark-gfmも必要らしい。

https://github.com/remarkjs/react-markdown#use


シンタックスハイライトも別途プラグインが必要らしい

https://github.com/remarkjs/react-markdown?tab=readme-ov-file#use-custom-components-syntax-highlight


できたけれど、なんとなくimportの仕方が気持ち悪い気がする。
Light Buildが用意されていた。

- https://github.com/react-syntax-highlighter/react-syntax-highlighter?tab=readme-ov-file#light-build

## 参考

- https://github.com/remarkjs/react-markdown
- https://gist.github.com/podhmo/41580702ce448710dcbcc6e06dbec548
- https://docs.deno.com/runtime/reference/jsx/
- https://github.com/react-syntax-highlighter/react-syntax-highlighter?tab=readme-ov-file#light-build

----------------------------------------

# Hello

This is a paragraph.

- This is a list item
- This is another list item
- This is a third list item

## This is a subheading

This is another paragraph.**Bold text** and *italic text*.

```js
console.log("Hello, world!");
```

This is a [link](https://example.com).

| Header 1 | Header 2 |
|----------|----------|
| Cell 1   | Cell 2   |
| Cell 3   | Cell 4   |
