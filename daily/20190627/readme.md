## github api

```
$ http https://api.github.com/users/podhmo
$ http https://api.github.com/users/podhmo/events
```

graphqlなどは認証不要？

### graphql

```console
$ cat 01*.json | http -a podhmo -b --json POST https://api.github.com/graphql
{
    "documentation_url": "https://developer.github.com/v3/auth#working-with-two-factor-authentication",
    "message": "Must specify two-factor authentication OTP code."
}
```


## emacsでcotrol sequenceを直接入力する

- https://www.wagavulin.jp/entry/2017/02/07/191302

1がa

## Invalid string: control characters from U+0000 through U+001F must be escaped

```console
$ cat x.json | jq .
parse error: Invalid string: control characters from U+0000 through U+001F must be escaped at line 13, column 2

$ cat x.json | tr -d '[:cntrl:]' | jq .
```

## graphql + httpie

その前にqueryの作成はどうすれば

- https://developer.github.com/v4/guides/using-the-explorer/
- https://developer.github.com/v4/explorer/
- https://www.howtographql.com/graphql-js/8-filtering-pagination-and-sorting/
- https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line


