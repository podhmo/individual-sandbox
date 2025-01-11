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
:memo: あとでcompileOptionsを読めるようにしないとだめかも？

以下の様なモジュールのimportが上手くいかない

- `import {...} from "@/components/ui/avatar"`
- `import { cn } from "@/lib/utils.ts"`
- `import {} from "lucide-react"`

tsconfig.jsonの設定に以下のような設定がある。なので`@/components/ui/avatar`はそのまま`./components/ui/avatar` なのだけれどcomponentsディレクトリにそのようなものが用意されていない。おそらくinit的なもので生成される何かなのだろう。

```json
{
  "compilerOptions": {
    "baseUrl": ".",
    "paths": {
      "@/*": ["./*"]
    }
  }
}
```

### @/components/ui/avatar

https://ui.shadcn.com/docs/components/button

↓のようにしてやるかコピペをして持ってくるらしい。不便では？ そしてこれの実行にはnext.jsのプロジェクトとpackage.jsonが必要らしい。不便では？

```
pnpm dlx shadcn@latest add button
```

### @/lib/utils.ts

どうやらnext.jsのプロジェクトで生成したときのutilsに含まれているらしい。だるい。。
ここでtailwindが出てくるのか。。

```ts
import { clsx, type ClassValue } from "npm:clsx"
import { twMerge } from "npm:tailwind-merge"

export function cn(...inputs: ClassValue[]) {
  return twMerge(clsx(inputs))
}
```

生成されたpackage.jsonを覗くとreact@18を使う必要があるかもしれない？

package.json

<details>

```json
{
  "name": "my-app",
  "version": "0.1.0",
  "private": true,
  "scripts": {
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint"
  },
  "dependencies": {
    "class-variance-authority": "^0.7.1",
    "clsx": "^2.1.1",
    "lucide-react": "^0.471.0",
    "next": "14.2.16",
    "react": "^18",
    "react-dom": "^18",
    "tailwind-merge": "^2.6.0",
    "tailwindcss-animate": "^1.0.7"
  },
  "devDependencies": {
    "@types/node": "^20",
    "@types/react": "^18",
    "@types/react-dom": "^18",
    "eslint": "^8",
    "eslint-config-next": "14.2.16",
    "postcss": "^8",
    "tailwindcss": "^3.4.1",
    "typescript": "^5"
  }
}
```

</details>

## tailwindを剥がす必要がある？

そもそも幾つか知らないパッケージがある

- npm:class-variance-authority https://cva.style/docs
- npm:clsx
- npm:tailwind-merge

一旦それは置いておいてmigrationをする

- https://twind.dev/handbook/the-shim.html#basic-usage
- https://github.com/tw-in-js/twind/tree/main/examples/basic

とりあえずそれっぽくつなげたけれどまだ残っている。

```
10:13:46.701 scheduler.development.mjs:2 [TWIND_INVALID_CLASS] Unknown class "size-4" found. {detail: 'size-4'}
10:13:46.715 scheduler.development.mjs:2 [TWIND_INVALID_CLASS] Unknown class "lucide" found. {detail: 'lucide'}
10:13:46.716 scheduler.development.mjs:2 [TWIND_INVALID_CLASS] Unknown class "lucide-message-circle" found.
10:13:46.717 scheduler.development.mjs:2 [TWIND_INVALID_CLASS] Unknown class "lucide-repeat2" found. {detail: 'lucide-repeat2'}
10:13:46.719 scheduler.development.mjs:2 [TWIND_INVALID_CLASS] Unknown class "lucide-heart" found. {detail: 'lucide-heart'}
```

