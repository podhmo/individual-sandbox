## node.js require

これを動かそうとしていた。

https://asciidwango.github.io/js-primer/use-case/nodecli/read-file/#error-handling

```console
$ node
> require("commander");
require("commander");
Error: Cannot find module 'commander'
    at Function.Module._resolveFilename (internal/modules/cjs/loader.js:581:15)
    at Function.Module._load (internal/modules/cjs/loader.js:507:25)
    at Module.require (internal/modules/cjs/loader.js:637:17)
    at require (internal/modules/cjs/helpers.js:20:18)
```

めんどくさいのでglobalに入れちゃう。

```console
$ npm install -g commander
$ npm link commander
```

## python jupyter nteract試す

https://github.com/nteract/nteract


```console
$ pip install nteract_on_jupyter
$ jupyter-nteract <>.ipynb --ip 127.0.0.1
```
