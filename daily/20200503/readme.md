## go sum type

- https://qiita.com/sxarp/items/cd528a546d1537105b9d
- https://medium.com/@haya14busa/sum-union-variant-type-in-go-and-static-check-tool-of-switch-case-handling-3bfc61618b1e
- https://making.pusher.com/alternatives-to-sum-types-in-go/
- https://eli.thegreenplace.net/2018/go-and-algebraic-data-types/

いろいろな方法がありそう

- "sum type" interface
- visitor pattern


## metashape python

いろいろ調整してpythonのクラスからgoのstructを生成できようにしてみる。
ただ、これが大元じゃなかったはず。昨日は途中からprestringの調整をやっていた。

### とりあえず荒削りでstructの出力

walkerのおかげでクラスをたどるのは楽なのだった。markしない状況も気にできるようにしたのだった。
あとは、composite type的なものへの対応をいい感じにやるための調整をしていたのだった。

### prestringの調整

結局昨日のprestringいじりは何が変わったのだっけ？

- import_が何も生成しない状態でも `import (` などを出力しないようにした
- というか、そういう機構をいい感じに調整できるようにした
- メソッドの名前をいい感じにした
- goの出力を調整した

### 追記

不足していそうなこと

- import付きのcomposite type
- docstring
- private field
- union type
- inheritance

