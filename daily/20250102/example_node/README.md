# 素直にnodeでreact-routerを試す

↓でdeno + esm.shでreact-routerを試してみたら結構辛かった

- https://gist.github.com/podhmo/86649d8ef158a5b903fbd866c18d9d5c

こんな感じの印象

- react-router@7はreact@19に依存していそう？(18だと上手く動かない）
- react@19はesm.shだとdevelopmentモードじゃないと動かない
- deps=react@18,react-dom@18と指定しても上手く動いてくれない

というわけで素直にnodeで試してみることにする

## 00 init

https://reactrouter.com/start/framework/installation

```console
$ npx create-react-router@latest foo
```

create-react-router@7.1.1 がインストールされたけれど、depsの部分のインストールで時間が掛かってた。

どうやら以下のようなかんじみたい

- vite.config.ts が生成されてる
    - tailwindcssとautoprefixerがcss用のpluginとして設定されている
    - reactRouterとtsConfigPathsがpluginとして設定されている

- package-lock.json
    - react,react-domは`^19.0.0`が指定されている
        - schedulerは0.2.5
        - reactは19.0.0
        - react-domは19.0.0

もしかして、18.0では動かない？

https://reactrouter.com/start/library/installation ここを見るとreact@18でもいけそうなかんじなのだけど。



> React Router v7 requires the following minimum versions:
> 
>  -  node@20
>  -  react@18
>  -  react-dom@18



package.jsonは以下のような感じ。これはフレームワークとしてのreact-routerのようなきがする？

```json
{
  "name": "foo",
  "private": true,
  "type": "module",
  "scripts": {
    "build": "cross-env NODE_ENV=production react-router build",
    "dev": "react-router dev",
    "start": "cross-env NODE_ENV=production react-router-serve ./build/server/index.js",
    "typecheck": "react-router typegen && tsc"
  },
  "dependencies": {
    "@react-router/node": "^7.1.1",
    "@react-router/serve": "^7.1.1",
    "isbot": "^5.1.17",
    "react": "^19.0.0",
    "react-dom": "^19.0.0",
    "react-router": "^7.1.1"
  },
  "devDependencies": {
    "@react-router/dev": "^7.1.1",
    "@types/node": "^20",
    "@types/react": "^19.0.1",
    "@types/react-dom": "^19.0.1",
    "autoprefixer": "^10.4.20",
    "cross-env": "^7.0.3",
    "postcss": "^8.4.49",
    "tailwindcss": "^3.4.16",
    "typescript": "^5.7.2",
    "vite": "^5.4.11",
    "vite-tsconfig-paths": "^5.1.4"
  }
}
```

app/root.tsxがentry pointなんだろうか？
全然関係ないけれど、ErrorBoundary()は定義しておいた方が良いのかもしれない。


tsconfig.jsonはこんなかんじか。libはDOM,DOM.Iterable,ES2022。

<details>

```json
{
  "include": [
    "**/*",
    "**/.server/**/*",
    "**/.client/**/*",
    ".react-router/types/**/*"
  ],
  "compilerOptions": {
    "lib": ["DOM", "DOM.Iterable", "ES2022"],
    "types": ["node", "vite/client"],
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "bundler",
    "jsx": "react-jsx",
    "rootDirs": [".", "./.react-router/types"],
    "baseUrl": ".",
    "paths": {
      "~/*": ["./app/*"]
    },
    "esModuleInterop": true,
    "verbatimModuleSyntax": true,
    "noEmit": true,
    "resolveJsonModule": true,
    "skipLibCheck": true,
    "strict": true
  }
}
```

</details>

react-router.config.tsとかも設定されているのか。。

```ts
import type { Config } from "@react-router/dev/config";

export default {
  // Config options...
  // Server-side render by default, to enable SPA mode set this to `false`
  ssr: true,
} satisfies Config;
```

## 01 routing

https://reactrouter.com/start/framework/routing
