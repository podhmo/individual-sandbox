## openapi parser

真面目に書くか。

### 追記

何か混ざっているような気がする。

これは嫌な前提

- schemasをloopするとすべてのschemaが辿れる（と思っている)

嫌な理由は、responsesやapi一つだけを見た時に、tree shaking的な事が起きないから。

### 追記

これもあまり良くない前提な気がする。

- schemaが必要かどうかを判断するために、最も末尾のrefまで再帰的に展開してアクセスしようとする
- refのrefを展開していく

これは単にrefを一段紐解いてqueueに追加すれば良いだけ。
(１つ気になるのが、後々利用するところでもrefを再帰的に展開する必要が出てきそうな点。それは終盤でnormalizeしてあげれば良いのでは？)


### 追記

arrayの扱いはどうだろうか？

- Arrayがあったというイベントとして残しておくのはありかもしれない
- その上で、typesには追加しなければ良いのでは？
- unknownも別途保持しておくのが無難では？
