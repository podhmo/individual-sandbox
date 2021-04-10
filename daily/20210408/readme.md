## go validator

- とりあえずfield checkを追加した
- 気づいたけれどtype単位でのvalidationが必要だ

  - これをいつ判定してあげると良いのだろう？
  - structならdescribeの対象にならないとダメなのでは？

- pathは常に欲しいな。validation時にpathを表示したい

やること

- ok type毎にvalidationを追加
- ok validation errorにpathを追加
- ok errorを見つけても継続できるようにする
- map, slice に対応する
- contextを受け取るようにする
- 順序に依存をもたせたくない
- 自己参照

### schemagen

- new typeを判定することは不可能なのだっけ？Validationを持っていたらそれを呼ぶくらいが無難かも
- oneOfどうすると良いんだろう？

  - interfaceを登録して上書きしてあげられると嬉しいかも？

