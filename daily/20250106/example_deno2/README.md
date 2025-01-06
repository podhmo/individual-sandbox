# reactのいろいろなライブラリをesm.sh経由で実行できるか試してみたい

initして色々試してみる

```console
$ deno run jsr:@podhmo/glue@0.2.3 init
```

## 00 markdown

どうやらreact-markdownはreact@18にしか対応していないみたい？ `react-is@18.3.1` を変更することはできない？

```
    "react-markdown@8.0.7_@types+react@19.0.2_react@19.0.0": {
      "integrity": "sha512-bvWbzG4MtOU62XqBx3Xx+zB2raaFFsq4mYiAzfjXJMEz2sixgeAfraA3tvzULF02ZdOMUOKTBFFaZJDDrq+BJQ==",
      "dependencies": [
        "@types/hast",
        "@types/prop-types",
        "@types/react",
        "@types/unist",
        "comma-separated-tokens",
        "hast-util-whitespace",
        "prop-types",
        "property-information",
        "react",
        "react-is@18.3.1",
        "remark-parse",
        "remark-rehype",
        "space-separated-tokens",
        "style-to-object",
        "unified",
        "unist-util-visit",
        "vfile"
      ]
    },
```

仕方がないのでreact@18にするか。。

## 01 readmeをAPIから取ってくる

このgistにアップロードしたので取ってこれるはず。

https://gist.githubusercontent.com/podhmo/395689b310af88566f1df31ed218592d/raw/74f9ad1c6897a9d32875338de8734b32383b9aab/README.md


一度react@19でdeno.lockを作ってしまうとreact-markdownから参照されるURLがおかしくなるみたい。

http "https://esm.sh/react-markdown@8.0.7?deps=@types/debug@4.1.12,@types/hast@2.3.10,@types/mdast@3.0.15,@types/ms@0.7.34,@types/prop-types@15.7.14,@types/react@19.0.2,@types/unist@2.0.11,bail@2.0.2,character-entities@2.0.2,comma-separated-tokens@2.0.3,csstype@3.1.3,debug@4.4.0,decode-named-character-reference@1.0.2,dequal@2.0.3,diff@5.2.0,extend@3.0.2,hast-util-whitespace@2.0.1,inline-style-parser@0.1.1,is-buffer@2.0.5,is-plain-obj@4.1.0,js-tokens@4.0.0,kleur@4.1.5,loose-envify@1.4.0,mdast-util-definitions@5.1.2,mdast-util-from-markdown@1.3.1,mdast-util-to-hast@12.3.0,mdast-util-to-string@3.2.0,micromark-core-commonmark@1.1.0,micromark-factory-destination@1.1.0,micromark-factory-label@1.1.0,micromark-factory-space@1.1.0,micromark-factory-title@1.1.0,micromark-factory-whitespace@1.1.0,micromark-util-character@1.2.0,micromark-util-chunked@1.1.0,micromark-util-classify-character@1.1.0,micromark-util-combine-extensions@1.1.0,micromark-util-decode-numeric-character-reference@1.1.0,micromark-util-decode-string@1.1.0,micromark-util-encode@1.1.0,micromark-util-html-tag-name@1.2.0,micromark-util-normalize-identifier@1.1.0,micromark-util-resolve-all@1.1.0,micromark-util-sanitize-uri@1.2.0,micromark-util-subtokenize@1.1.0,micromark-util-symbol@1.1.0,micromark-util-types@1.1.0,micromark@3.2.0,mri@1.2.0,ms@2.1.3,object-assign@4.1.1,prop-types@15.8.1,property-information@6.5.0,react-is@16.13.1,react-is@18.3.1,react@19.0.0,remark-parse@10.0.2,remark-rehype@10.1.0,sade@1.8.1,space-separated-tokens@2.0.2,style-to-object@0.4.4,trim-lines@3.0.1,trough@2.2.0,unified@10.1.2,unist-util-generated@2.0.1,unist-util-is@5.2.1,unist-util-position@4.0.4,unist-util-stringify-position@3.0.3,unist-util-visit-parents@5.1.3,unist-util-visit@4.1.2,uvu@0.5.6,vfile-message@3.1.4,vfile@5.3.7"

長すぎるともしかしてエラーになる？

http "https://esm.sh/react-markdown@8.0.7?deps=@types/debug@4.1.12,@types/hast@2.3.10,@types/mdast@3.0.15,@types/ms@0.7.34,@types/prop-types@15.7.14,@types/react@18.3.18,@types/unist@2.0.11,bail@2.0.2,character-entities@2.0.2,comma-separated-tokens@2.0.3,csstype@3.1.3,debug@4.4.0,decode-named-character-reference@1.0.2,dequal@2.0.3,diff@5.2.0,extend@3.0.2,hast-util-whitespace@2.0.1,inline-style-parser@0.1.1,is-buffer@2.0.5,is-plain-obj@4.1.0,js-tokens@4.0.0,kleur@4.1.5,loose-envify@1.4.0,mdast-util-definitions@5.1.2,mdast-util-from-markdown@1.3.1,mdast-util-to-hast@12.3.0,mdast-util-to-string@3.2.0,micromark-core-commonmark@1.1.0,micromark-factory-destination@1.1.0,micromark-factory-label@1.1.0,micromark-factory-space@1.1.0,micromark-factory-title@1.1.0,micromark-factory-whitespace@1.1.0,micromark-util-character@1.2.0,micromark-util-chunked@1.1.0,micromark-util-classify-character@1.1.0,micromark-util-combine-extensions@1.1.0,micromark-util-decode-numeric-character-reference@1.1.0,micromark-util-decode-string@1.1.0,micromark-util-encode@1.1.0,micromark-util-html-tag-name@1.2.0,micromark-util-normalize-identifier@1.1.0,micromark-util-resolve-all@1.1.0,micromark-util-sanitize-uri@1.2.0,micromark-util-subtokenize@1.1.0,micromark-util-symbol@1.1.0,micromark-util-types@1.1.0,micromark@3.2.0,mri@1.2.0,ms@2.1.3,object-assign@4.1.1,prop-types@15.8.1,property-information@6.5.0,react-is@16.13.1,react-is@18.3.1,react@18.3.1,remark-parse@10.0.2,remark-rehype@10.1.0,sade@1.8.1,space-separated-tokens@2.0.2,style-to-object@0.4.4,trim-lines@3.0.1,trough@2.2.0,unified@10.1.2,unist-util-generated@2.0.1,unist-util-is@5.2.1,unist-util-position@4.0.4,unist-util-stringify-position@3.0.3,unist-util-visit-parents@5.1.3,unist-util-visit@4.1.2,uvu@0.5.6,vfile-message@3.1.4,vfile@5.3.7"

これくらいなら通る。

http "https://esm.sh/react-markdown@8.0.7?deps=react@18.3.1"

## 02 MUI

muiとかはそのまま動くんだろうか？

https://mui.com/material-ui/getting-started/usage/

こちらもダメそうだ。。

```
import Button from "/@mui/material@6.3.1/Button?deps=@babel/runtime@7.26.0,@emotion/cache@11.14.0,@emotion/hash@0.9.2,@emotion/memoize@0.9.0,@emotion/serialize@1.3.3,@emotion/sheet@1.4.0,@emotion/unitless@0.10.0,@emotion/utils@1.4.2,@emotion/weak-memoize@0.4.0,@mui/core-downloads-tracker@6.3.1,@mui/private-theming@6.3.1,@mui/styled-engine@6.3.1,@mui/system@6.3.1,@mui/types@7.2.21,@mui/utils@6.3.1,@popperjs/core@2.11.8,@types/prop-types@15.7.14,@types/react-transition-group@4.4.12,@types/react@18.3.18,clsx@2.1.1,csstype@3.1.3,dom-helpers@5.2.1,js-tokens@4.0.0,loose-envify@1.4.0,object-assign@4.1.1,prop-types@15.8.1,react-dom@18.3.1,react-is@16.13.1,react-is@19.0.0,react-transition-group@4.4.5,react@18.3.1,regenerator-runtime@0.14.1,scheduler@0.23.2,stylis@4.2.0";
```


## 03 v0

こういうので行けるかと思ったがダメそうだった。next.js用なんだろうか？ package.jsonがない場合にはNext.jsのプロジェクトを作らされる。そしてそこで失敗する。

```
deno run -A npm:shadcn@latest add "https://v0.dev/chat/b/b_HB9HL0PwJbJ"
...
does not contain a package.json file. Would you like to start a new Next.js project? … yes
✔ What is your project named? … my-app
⠋ Creating a new Next.js project. This may take a few minutes.┏ ⚠️  Deno requests run access to "npx".
┠─ Requested by `Deno.Command().spawn()` API.
┠─ To see a stack trace for this prompt, set the DENO_TRACE_PERMISSIONS environmental variable.
┠─ Learn more at: https://docs.deno.com/go/--allow-run
┠─ Run again with --allow-run to bypass this prompt.
┗ Allow? [y/n/A] (y = yes, allow; n = no, deny; A = allow all run permissions) > A


Something went wrong creating a new Next.js project. Please try again.
```

仕方がないのでnpxで実行したらなんか作られた。あんまり興味がないのでこの辺でおしまい。

```
my-app/
├── README.md
├── app
│   ├── favicon.ico
│   ├── fonts
│   │   ├── GeistMonoVF.woff
│   │   └── GeistVF.woff
│   ├── globals.css
│   ├── layout.tsx
│   └── page.tsx
├── components
│   ├── Timeline.tsx
│   ├── Tweet.tsx
│   ├── TweetInput.tsx
│   └── ui
│       ├── avatar.tsx
│       ├── button.tsx
│       └── textarea.tsx
├── components.json
├── lib
│   └── utils.ts
├── next.config.mjs
├── package-lock.json
├── package.json
├── postcss.config.mjs
├── tailwind.config.ts
└── tsconfig.json
```

tailwindだけを実行する方法などは把握しておくと良いかもしれない。たぶんラッパー系のものは使えない。

