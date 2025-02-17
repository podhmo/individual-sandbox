# esbuildでのcssの扱いを調べたい

たしかjsからimportしたときにも良いかんじに扱われる。はず。

https://esbuild.github.io/content-types/#css

- bundle css `esbuild --bundle app.css --outfile=out.css`
- from js `esbuild --bundle --outfile=app.js hello.js`
- css modules (defaultでは `<name>.module.css`')

```console
$ make

.
├── Makefile
├── README.md
├── base.css
├── code.module.css
├── dist
│   ├── app.css
│   └── app.js
├── hello.js
└── index.html
```

## bundle css

単にbundleされるだけ

## from js

jsの中でcssをimportするとjsと同名のprefixをファイル名として持つcssが生成される(app.js, app.css)。

## css modules

`<name>.module.css` という形式のファイルをjsからimportすると良いかんじにファイル名をprefixとして使う code.module.css の .normal が .code_normal に変換される。
