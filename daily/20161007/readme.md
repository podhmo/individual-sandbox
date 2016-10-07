# go go-githubの仕組みを調べる

- gists.go
- gists_test.go
- gists_comments.go
- gists_comments_test.go

## gists.go

- responseの型を定義(Gist)
- requestの型を定義GistFilename, GistFile, GistCommit, GistFork, GistListOptions
- serviceのmethodとしてList,ListAll,ListStared,Get,GetRevision,Create,Edit,ListCommits,Delete,Star,Unstar,IsStarrred,Fork,ListForks

### methodの中身

```go
func (s *GistsService) <method>(id string) (<data>, *Response, error) {
	u := // urlを作成
	req, err := s.client.NewRequest(<GET,POST>, u, nil)
	if err != nil {
		return nil, nil, err
	}

    // s.client.Doを実行
	resp, err := s.client.Do(req, g)
	if err != nil {
		return nil, resp, err
	}
    // dataが必要なら変換
    data := convert(resp)
    // dataと一緒にraw responseも返す
	return g, resp, err
}
```

`client.Do()` とは？

- serviceはclientを持っている
- [github.go](https://github.com/google/go-github/blob/master/github/github.go)に色々な情報がある。

# gists_test.go

- mux.HandleFuncで作っているっぽい。
- setup,teardownは[この辺](https://github.com/google/go-github/blob/master/github/github_test.go)見ると良い。
