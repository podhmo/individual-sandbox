# denoを使ってgistに長文を投稿しそのURLを利用したい

bskyやx/twitterへの投稿のときに文字数制限に引っかかることがある。
とくにGeminiやChatGPTなどで文字列を増幅させると引っかかる。こういうときに別の場所に追加部分を保存して共有したい。

## 認証は？

`gh auth token` とかでごまかせないだろうか？こういうふうに依存が増えると自分の環境用のdoctor的なコマンドが欲しくなるかもしれない。

### gistへの投稿は？

createはこれで行けるのか？

```console
$ gh gist create --public --desc "create gist by gh" README.md --web
- Creating gist README.md
* Request at 2024-12-03 21:35:48.207289236 +0900 JST m=+0.028746012
* Request to https://api.github.com/gists
* Request took 1.198777937s
✓ Created gist README.md
https://gist.github.com/podhmo/fa9434d109cbd67d3cf87a869409738f
```

updateはこんな感じ？１つのファイルしか対象にできないんだろうか？

```console
$ gh gist edit https://gist.github.com/podhmo/fa9434d109cbd67d3cf87a869409738f README.md
```

## 01 subprocessを使ってghコマンドで投稿する

標準入力から渡す部分はちょっと手こずった。あと完全にDenoに依存することになりそう。

```console
$ deno run -A main.ts --content "hello world" --debug --public
- Creating gist README.md
✓ Created public gist README.md
{
  stdout: [ "https://gist.github.com/podhmo/ac11978c4eb63c9d722c991654e90105" ]
}
gist url: https://gist.github.com/podhmo/ac11978c4eb63c9d722c991654e90105
```

## 02 REST API経由で投稿する

こちらのほうが綺麗かもしれない。`gh auth token` を.envに仕込んでおけば良さそう。

```console
$ deno run --allow-net --allow-read --allow-env main2.ts --filename=README.md --content="hello gist"
created a gist: https://gist.github.com/podhmo/4ecb91b5638c7d5f09a94baaee3af28a
```