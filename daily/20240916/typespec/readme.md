# typepec

https://typespec.io/docs

openapi.yamlをつくる。


setup

```console
$ npm install @typespec/compiler
$ tsp install
$ tsp init # REST APIとかl選ぶとpackage.jsonなどが生成される
```

build

```console
$ npm run build
...
$ tree
.
├── main.tsp
├── package-lock.json
├── package.json
├── readme.md
├── tsp-output
│   └── @typespec
│       └── openapi3
│           └── openapi.yaml
└── tspconfig.yaml

4 directories, 6 files
```

