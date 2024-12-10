# bskyにスレッドで投稿したい

とりあえず投稿するだけ。

## documentを覗く

- https://docs.bsky.app/docs/api/com-atproto-repo-create-record
  - recordのschemaが書かれていない
- [no record schema in the docs · Issue #120 · bluesky-social/bsky-docs](https://github.com/bluesky-social/bsky-docs/issues/120)
  - https://github.com/bluesky-social/atproto/blob/e9240c8b5e6a62d5f83c1825002046c45d28b055/lexicons/app/bsky/feed/post.json#L9


## 無理やりスレッドにする

どうやらreplyに渡してあげれば良いみたい。uri,cidが何を意味するものかを正確には把握していない（片方ではダメな理由は？）

```ts
reply: { // https://github.com/bluesky-social/atproto/blob/main/lexicons/com/atproto/repo/strongRef.json
    root: {
        uri: "at://did:plc:hpog7qvhzybjzzjq3p5eq6ei/app.bsky.feed.post/3lcv345ggho2w",
        cid: "bafyreidydm6jeszhue7wqemkbsb4gkqm2mk6kcgy35o4blt6mptnm6mnsu",
    },
    parent: {
        uri: "at://did:plc:hpog7qvhzybjzzjq3p5eq6ei/app.bsky.feed.post/3lcv345ggho2w",
        cid: "bafyreidydm6jeszhue7wqemkbsb4gkqm2mk6kcgy35o4blt6mptnm6mnsu",
    },
},
```