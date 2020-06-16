## graphql

- http://apis.guru/graphql-apis/

## go graphql-go

- github.com/graphql-go/graphql
- github.com/graphql-go/handler

## git work-tree

- https://qiita.com/shibukk/items/80430b54ecda7f36ca44

## git 特定のディレクトリだけをclone

sparse checkoutを使う

```console
$ git clone --filter=blob:none --no-checkout <url>
$ git sparse-checkout init --cone
$ git sparse-checkout set <dir> ...
```

setの代わりにaddでも良い

### 追記

以下は古い方法

```console
$ mkdir foo && cd foo
$ git init
$ git config
$ git remote add origin <url>
$ echo <dir> >> .git/sparse-checkout
$ git pull origin master
```

もっと手軽な方法

```console
$ git clone --no-checkout <url>
$ echo <dir> >> .git/sparse-checkout
$ git pull origin master
```

## go sandbox example

- users,books,tags
- 新規登録時にslack通知
- webAPIが存在
- tests
- (social連携)
- slackbotとしてamazonのURLをparseして整形 -> 登録
- API serverとしてdeploy
- slack botとして起動
- sqs経由でのtrigger
- oauth proxyで特定の人以外制限

これで何を見たいのか？

## go io_uring

- https://developers.mattermost.com/blog/hands-on-iouring-go/
