# denoでいい感じにコマンドライン引数を解析したい

- `jsm:@std/cli.parseArgs()`
  - `import {parseArgs} from "@std/cli/parse-args"`
  - https://jsr.io/@std/cli/parse
- `node:util.parseArgs()`
  - `import {parseArgs} from "node:util"`
  - https://docs.deno.com/api/node/util/~/parseArgs

## `jsm:@std/cli.parseArgs()` の方

examplesにも例が載っている。

https://docs.deno.com/examples/command-line-arguments/

コード例

```ts
import { parseArgs } from "jsr:@std/cli/parse-args";

const flags = parseArgs(Deno.args, {
  boolean: ["help", "color"],
  string: ["version"],
  default: { color: true },
  negatable: ["color"],
});

// deno run deno-cli-parse-args.ts --color=false --version=1.0.0
// deno run deno-cli-parse-args.ts --no-color --version=1.0.0
console.dir(flags, { depth: null});
```

特徴

- boolean, string のフラグの名前を設定する (`--boolean=true,--boolean=false`で指定も可能)
- デフォルト値を指定できる
- 値を反転させたさせたフラグを作成できる (e.g. `--no-color`)
- ある程度型がつく `string | undefined` になる
- (boolean は `boolean | undefined` ではなく `boolean` になる)

気になるところ

- `--help`や`-h`でのヘルプメッセージが存在しない
- (`@std` prefixだけれどstandardではなさそう..)
- `string | undefined` を `string` にするために自分でコードを書かないといけない
- (boolean, stringの記述の仕方が好きではない)
- 余分な引数は`_` でアクセス
- typoしたフラグもtypoしたキーとしてオブジェクトに格納される (e.g. `{colr: true}`)

## `node:util.parseArgs()` の方

node.jsの標準ライブラリに入っている

https://nodejs.org/api/util.html#utilparseargsconfig


```ts
import { parseArgs } from "node:util";

const { values, positionals } = parseArgs({
    options: {
        "no-color": { type: 'boolean', default: false },
        version: { type: 'string' }
    },
    allowPositionals: true
});
const flags = { ...values, color: !values["no-color"] };
```

特徴

- (denoの方より使われているコードが多そう..)
- フラグ毎にキーを持たせたオブジェクトでオプションを指定する。個人的にはわかりやすい。
- multiple:trueで複数回書くことで配列に対応してる
- aliasをつけられる
- short nameをつけられる
- 複数回呼び出す
- strict:true で 知らないフラグが指定された時にエラーになる (defaultでtrue)

気になるところ

- `--help`や`-h`でのヘルプメッセージが存在しない
- (`node:util` のimportがキモい（気持ちの問題）)
- (戻り値の仕様が分かりづらい。分配束縛で値を取り出すと変数名がvaluesになるのがキモい `const {values: flags} = ` とかできるけど)
- `boolean | undefined` に defaultを付けても `boolean` にならない
- booleanのフラグに対して値を反転して設定するようなオプションがない (e.g. `negatable: true`)
- フラグをタイポしたときのエラーは分かりやすくはない

> [!NOTE]
> フラグをタイポしたときのエラーは分かりやすくはない
> 
> ```console
> $ deno run node-util-parse-args.ts --color=false --version=1.0.0
> error: Uncaught (in promise) TypeError: Option '--color' does not take an argument
> const { values: flags } = parseArgs({
>                           ^
>     at checkOptionUsage (ext:deno_node/internal/util/parse_args/parse_args.js:114:11)
>     at ext:deno_node/internal/util/parse_args/parse_args.js:404:9
>     at Array.forEach (<anonymous>)
>     at parseArgs (ext:deno_node/internal/util/parse_args/parse_args.js:401:3)
>     at file:///Users/podhmo/ghq/github.com/podhmo/individual-sandbox/daily/20241024/example_deno/node-util-parse-args.ts:3:27
> ```

## 欲しい機能

個人的には以下の機能が欲しいみたい

- `-h`や`--help`でのヘルプメッセージ (e.g. https://doc.rust-lang.org/cargo/commands/cargo.html )
- requiredなフラグを設定可能 かつ `V | undefined` ではなく `V` として扱いたい
- 真偽が逆転したフラグ (negatable)
- 配列を指定可能

以下は要らない

- 複数の型に対応したparser (個人的にはstringとbooleanだけで良い)
- メソッドチェインとかで良い感じに記述するインターフェイス

### ヘルプメッセージ

ヘルプメッセージはこんな感じで表示して欲しい

```
 $ deno-cli-parse-args.ts --help
 Description of deno-cli-parse-args
 
 Usage: deno-cli-parse-args [OPTIONS] <PATH>
 
 OPTIONS:
     -v, --version      Set version (required)
     --no-color         Run without color
```

https://scrapbox.io/amutake/%E3%82%B3%E3%83%9E%E3%83%B3%E3%83%89%E3%81%AE_usage_%E3%81%AE%E6%9B%B8%E3%81%8D%E6%96%B9

### 捕捉

pythonのargparseは頑張ればわりと満足する表示になる ./python-argparse.py

```console
$ python python-argparse.py 
usage: python-argparse [-h] --version VERSION [--no-color] [positionals ...]

description of python-argparse

positional arguments:
  positionals        Positional arguments (default: [])

options:
  -h, --help         show this help message and exit
  --version VERSION  Set version (default: None)
  --no-color         Disable color (default: True)
python-argparse: error: the following arguments are required: --version
```

flagをタイポしたとき

> python-argparse: error: unrecognized arguments: --no-colr
