## go go-webtest

[昨日](../20190727/readme.md)の続き。

### reflect_walk

main.goを増やしていく感じ。ある程度上手く言ったら移動していくか。

- そこそこ色々なcontainerに載せる
- reflectを網羅する
- bson.ObjectId, sqlか何かのものを見る
- 循環参照を気にする

#### container

不足しているものたち

- interface{}
- map

:thought_balloon: 考えてみるとresponseに対応するならそれはもうJSON化されているものじゃない？


### snapshot

そういえばstruct以外のものには対応していないかもなー。

```
"foo"
1
[1,2,3]
```

あたり
