# bundleにesbuildの方を使ってみる

(一時期は標準ライブラリにこだわってdeno bundleが無くなることを嘆いて `@deno/emit` あたりをいじろうとしていたけれど)
コマンドで使わないのだからesbuildを直接使っても良いんじゃないか？。

## bundle.ts with chunks.dist.ts

とりあえず、以下のloaderの例を真似して何かしらのソースをbundleしてみる。

https://github.com/lucacasonato/esbuild_deno_loader

色々思いつくところをあげていく

- bundle:trueにするとしっかりbundleされる。bundle:falseで何ができるのか全然わからない
- pluginsを読み込んでいる, loader,resolverが使われているらしい
- 普通にbundlerとして使う分には必要十分なのでは？
- (esbuildをnpmで入れるとdeno.lockが極端に増えるのがだるい)

気になるところ

- esbuild側の知識がない。buildとtransformは何が違うのだろう？
- resolverとloaderは何が違うのだろう？
- 他のプロジェクト上のdeno.jsonを読み込んでいい感じにbundleするコマンドとしては使えるんだろうか？
- esmにbundleするだけでなく.d.tsも生成させる事はできるんだろうか？

out of context

- emoji pluginなるものがある https://github.com/lucacasonato/esbuild_deno_loader/blob/main/examples/custom_scheme_plugin.ts

### 00 PluginOptionsを見てみる

plugin option？そもそもどうやって指定するんだろう？

https://github.com/lucacasonato/esbuild_deno_loader/blob/757170406bee0ec93cd5ddec92cf7af89cfc33b4/mod.ts#L23

- オプションに portable と native があるらしい
    - そういえば、portable loaderは上手くcacheを使えないとか書いてあった記憶
- configPathでdeno.jsonを直接指定できるらしい。
    - `--config` で指定せずともそれなりにいい感じに調べてくれるらしいがどんな挙動かはわからない

### 01 resolverを調べる

最初の過程はこの辺のような気がする。

> support npm:, jsr:

- https://github.com/lucacasonato/esbuild_deno_loader/blob/main/src/plugin_deno_resolver.ts

実際の関数自体はこの辺で定義されているものが使われているみたい

- https://github.com/lucacasonato/esbuild_deno_loader/blob/main/src/shared.ts
- findWorkspaceを利用してdeno.jsonの場所などを特定してconfigPathを設定してそう？

### 02 loaderを調べる

- https://github.com/lucacasonato/esbuild_deno_loader/blob/main/src/plugin_deno_loader.ts

## bundle.ts with with-help/mod.ts

外部のパッケージのファイルをそのまま読めるか試してみる。普通に行けそうだった。

気になったところは以下

- configPathを指定しなくてもいい感じにdeno.jsonを探して依存関係を見てくれる？
- そういえば出力されるのはbundleされたesmなので.d.tsは生成されるんだろうか？

with-help.dist.jsを作ってみた。これをhello.mjsから読み込んでみる。動いていそう。

```console
$ deno run hello.mjs --help
Usage: cli [options]

Description: 試しにbundleされたwith-helpを使ってみる

Options:
  --name    <string> (required) (default: name="world")
  --help    show help
```

## esbuild

- hello world
    - https://esbuild.github.io/getting-started/#build-scripts
- transpile, buildの違い
    - https://esbuild.github.io/api/#js-details
        - transformしてからbuildしている
    - build
        - entrypointから入力を受け取り結果をFSに書き出す
    - transform
        - 表現形式の変換 (e.g. ts -> js)
    - transpileは普通にts -> jsみたいな作業っぽい
    - buildはbundleを含んでいる？ (esbuildに`--bundle`オプションを指定しないとbundleはされない)
- plugins
    - https://esbuild.github.io/plugins/#using-plugins
    - 作成方法
        - hook的なものが発動するfilterを正規表現で書く
        - callbackを用意する
        - extenalを指定するとbundleの対象外にできるみたい？
            - 読むだけ読み込んでimportだけ書き換えたいかも？
    - simpleなpluginを作るということをしても良いのかもしれない
    - setupでいい感じにaddEventListenerしているような感じ

