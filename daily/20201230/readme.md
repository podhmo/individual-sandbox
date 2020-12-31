## 振り返り

とりあえず、今年1年で作ったrepositoryを見るか

- https://github.community/t/graphql-api-how-to-fetch-all-repositories-that-the-current-user-has-access-to/13792
- https://github.community/t/graphql-filtering-pull-request-on-createddate/13900
- https://qiita.com/toshi0607/items/c4e1635274ec51899bab
- https://docs.github.com/en/free-pro-team@latest/graphql/overview/explorer

## github cliの進化

https://github.com/cli/cli

- graphqlのAPIを呼べる
- MacPortsにも対応している `sudo port selfupdate && sudo port upgrade gh`
- completionがついている

### completion

補完の設定はこんな感じで見れるらしい。

```console
$ gh help completion
```

これを実行してあげれば良い

```console
$ eval $(gh completion -s bash)
```

### list all repository

makefile越しに実行してコレでハマっていた。`"`を使っていたので、"$endCursor"が展開されてしまっていたのか。

```
gh: Parse error on ":" (COLON) at [1, 7]
{
  "errors": [
    {
      "message": "Parse error on \":\" (COLON) at [1, 7]",
      "locations": [
        {
          "line": 1,
          "column": 7
        }
      ]
    }
  ]
}
```
