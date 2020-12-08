## python google api

- 直接JSONファイルを読み込んで実行
- Authorized sessionを利用
- refresh token -> access token

### refresh token -> access token

これはCredentialsのbefore_request()でrefresh()を呼んでくれる。

```
google.auth.credentials:Credentials <- builtins:object
    [method, OVERRIDE] __init__(self)
    [method] before_request(self, request, method, url, headers)
        [property] valid
        [method] apply(self, headers, token=None)
        [method] refresh(self, request)
    [property] expired
```

実際のところ google.auth/transport:AuthorizedSession などを使ってあげれば自動でよしなにやってくれる。

## apiのfake

- https://jsonplaceholder.typicode.com/
