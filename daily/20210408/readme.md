## go validator

- とりあえずfield checkを追加した
- 気づいたけれどtype単位でのvalidationが必要だ

  - これをいつ判定してあげると良いのだろう？
  - structならdescribeの対象にならないとダメなのでは？

- pathは常に欲しいな。validation時にpathを表示したい

やること

- type毎にvalidationを追加
- validation errorにpathを追加
- errorを見つけても継続できるようにする
- 順序に依存をもたせたくない
- contextを受け取るようにする

### schemagen

- new typeを判定することは不可能なのだっけ？Validationを持っていたらそれを呼ぶくらいが無難かも
- oneOfどうすると良いんだろう？

  - interfaceを登録して上書きしてあげられると嬉しいかも？
