## dictknife globalref,localref的な扱いを

- 意外とscannerが使える
- nameのconflictはruntime errorになるのはやりすぎ？

## go converter コンセプト的なプロトタイプ

- 可能な限りfieldを自動で生成して欲しい
- 自動で対応できない部分に対して値を挿入できるhookが欲しい
- hook中で不足している値があった時にbuildエラーになって欲しい
- 渡された設定を利用して値を計算したい
- 親の情報を利用して値を計算したい
