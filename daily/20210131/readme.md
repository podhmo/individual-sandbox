## go の書き方の整理

[../20210128/readme.md](../20210128/readme.md)の続き

- go のinterface

  - 一つだけメソッドを持つinterfaceのシンプルな定義と高階関数を利用したものについて
  - interfaceはactionが複数持てる。これと複数の関数ポインタを持ったstructとの違い
  - actionを複数実装した時の手間の問題

- interfaceの実装

  - 状態をもたせられる
  - アダプター的な挙動
  - IoC

- structural subtyping

  - 利用者側がどういう物が欲しいかの仕様を決められる
  - 提供者側がどのようなものを公開するか明示する必要がない
  - 後付の柔軟性、しかし既存の型への拡張に関してはこれが弱くなる
  - 既存の型への拡張を行った場合、必ず何かでwrapする必要が出てくる




