# v0 (shadcn/ui) の結果をhtml１枚にまとめてcodepenで共有したい

https://ui.shadcn.com/docs/installation/manual

- v0はshadcn/uiを利用する
- shadcn/uiはtailwindcssに依存している
    - tailwind.config.js をいじっている  
    - sassなども使われている？
- shadcn/uiの出力はradix uiに依存している？
- tsconfig.jsonでbaseUrlとpathsで"@/*"の設定をしている
- components.jsonをいじる

雑に言うとbuildが必要。これを不要な形にしたい。あるいはbuild結果を出力したい(bundleなし)。

## 先行事例

fresh経由でpreact + twindで頑張っている人は居た。

- [fresh (deno) で shadcn/ui を使う](https://zenn.dev/nikogoli/scraps/f80e1d4688d6b0)
- [fresh (deno) で shadcn/ui を使う 実践編](https://zenn.dev/nikogoli/scraps/207599bd096dff)

## v0のcomponentをとりあえず持ってくる

https://v0.dev/chat/twitter-like-timeline-PyGJMc4GJVR

:memo: `app/page.tsx` をgistに載せやすくする関係上 `app_page.tsx` という形に名前を変更して使う
