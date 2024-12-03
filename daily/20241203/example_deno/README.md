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
{
  "url": "https://api.github.com/gists/13631c4a1d97109474cdfa003acdc95b",
  "forks_url": "https://api.github.com/gists/13631c4a1d97109474cdfa003acdc95b/forks",
  "commits_url": "https://api.github.com/gists/13631c4a1d97109474cdfa003acdc95b/commits",
  "id": "13631c4a1d97109474cdfa003acdc95b",
  "node_id": "G_kwDOAAE65doAIDEzNjMxYzRhMWQ5NzEwOTQ3NGNkZmEwMDNhY2RjOTVi",
  "git_pull_url": "https://gist.github.com/13631c4a1d97109474cdfa003acdc95b.git",
  "git_push_url": "https://gist.github.com/13631c4a1d97109474cdfa003acdc95b.git",
  "html_url": "https://gist.github.com/podhmo/13631c4a1d97109474cdfa003acdc95b",
  "files": {
    "README.md": {
      "filename": "README.md",
      "type": "text/markdown",
      "language": "Markdown",
      "raw_url": "https://gist.githubusercontent.com/podhmo/13631c4a1d97109474cdfa003acdc95b/raw/e4d79ec064fb3a6bdc8d21c91848e6defd8384a2/README.md",
      "size": 10,
      "truncated": false,
      "content": "hello gist",
      "encoding": "utf-8"
    }
  },
  "public": false,
  "created_at": "2024-12-03T13:41:54Z",
  "updated_at": "2024-12-03T13:41:54Z",
  "description": null,
  "comments": 0,
  "user": null,
  "comments_url": "https://api.github.com/gists/13631c4a1d97109474cdfa003acdc95b/comments",
  "owner": {
    "login": "podhmo",
    "id": 80613,
    "node_id": "MDQ6VXNlcjgwNjEz",
    "avatar_url": "https://avatars.githubusercontent.com/u/80613?v=4",
    "gravatar_id": "",
    "url": "https://api.github.com/users/podhmo",
    "html_url": "https://github.com/podhmo",
    "followers_url": "https://api.github.com/users/podhmo/followers",
    "following_url": "https://api.github.com/users/podhmo/following{/other_user}",
    "gists_url": "https://api.github.com/users/podhmo/gists{/gist_id}",
    "starred_url": "https://api.github.com/users/podhmo/starred{/owner}{/repo}",
    "subscriptions_url": "https://api.github.com/users/podhmo/subscriptions",
    "organizations_url": "https://api.github.com/users/podhmo/orgs",
    "repos_url": "https://api.github.com/users/podhmo/repos",
    "events_url": "https://api.github.com/users/podhmo/events{/privacy}",
    "received_events_url": "https://api.github.com/users/podhmo/received_events",
    "type": "User",
    "user_view_type": "public",
    "site_admin": false
  },
  "forks": [],
  "history": [
    {
      "user": {
        "login": "podhmo",
        "id": 80613,
        "node_id": "MDQ6VXNlcjgwNjEz",
        "avatar_url": "https://avatars.githubusercontent.com/u/80613?v=4",
        "gravatar_id": "",
        "url": "https://api.github.com/users/podhmo",
        "html_url": "https://github.com/podhmo",
        "followers_url": "https://api.github.com/users/podhmo/followers",
        "following_url": "https://api.github.com/users/podhmo/following{/other_user}",
        "gists_url": "https://api.github.com/users/podhmo/gists{/gist_id}",
        "starred_url": "https://api.github.com/users/podhmo/starred{/owner}{/repo}",
        "subscriptions_url": "https://api.github.com/users/podhmo/subscriptions",
        "organizations_url": "https://api.github.com/users/podhmo/orgs",
        "repos_url": "https://api.github.com/users/podhmo/repos",
        "events_url": "https://api.github.com/users/podhmo/events{/privacy}",
        "received_events_url": "https://api.github.com/users/podhmo/received_events",
        "type": "User",
        "user_view_type": "public",
        "site_admin": false
      },
      "version": "8d791bf3d0c42b7a09762e0ffcaf0b2da857a866",
      "committed_at": "2024-12-03T13:41:54Z",
      "change_status": {
        "total": 1,
        "additions": 1,
        "deletions": 0
      },
      "url": "https://api.github.com/gists/13631c4a1d97109474cdfa003acdc95b/8d791bf3d0c42b7a09762e0ffcaf0b2da857a866"
    }
  ],
  "truncated": false
}
```