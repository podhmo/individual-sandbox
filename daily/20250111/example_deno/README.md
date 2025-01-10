# next.esm.shにアクセスしてMUIの例を動かしてみる

どうやらreact@19なら素直に動くみたい。react@18だとschedulerのあたりでreact19を利用してしまう？
これを上手く認識してくれる方法はないだろうか？

# code pen

- https://codepen.io/podhmo-the-animator/full/YPKaGeL

# あやしい部分

依存部分がreactとなっている部分で自動的にreact@18のコードが使われるが実際のコードはreact@19用みたいなことが起きてる？
react-isあたりで実行時に分岐していてそのバージョンがずれてるとかそう。

```
    "@mui/material@6.3.1_@types+react@18.3.18_react@18.3.1_react-dom@18.3.1__react@18.3.1": {
      "integrity": "sha512-ynG9ayhxgCsHJ/dtDcT1v78/r2GwQyP3E0hPz3GdPRl0uFJz/uUTtI5KFYwadXmbC+Uv3bfB8laZ6+Cpzh03gA==",
      "dependencies": [
        "@babel/runtime",
        "@mui/core-downloads-tracker",
        "@mui/system",
        "@mui/types",
        "@mui/utils",
        "@popperjs/core",
        "@types/react",
        "@types/react-transition-group",
        "clsx",
        "csstype",
        "prop-types",
        "react",
        "react-dom",
        "react-is@19.0.0",
        "react-transition-group"
      ]
    },

```

一応react@18にも対応してそうだけれど...

> https://mui.com/material-ui/getting-started/installation/

```
"peerDependencies": {
  "react": "^17.0.0 || ^18.0.0 || ^19.0.0",
  "react-dom": "^17.0.0 || ^18.0.0 || ^19.0.0"
},
```

CDNはおすすめされてなかった。この辺で十分だろう..

https://mui.com/material-ui/getting-started/installation/#cdn

>[!WARNING]
> We do not recommend using this approach in production. It requires the client to download the entire library—regardless of which components are actually used—which negatively impacts performance and bandwidth utilization.
