# python dictknifeを修正していた

adhocに名前を決めちゃいたい。

- 名前がない場合 "Error.yaml" -> ファイル名から
- definitions/xxxx みたいなやつ -> そのまま
- /xxxx みたいなやつ -> prefixを補う
- 即時定義 -> flatになった段階で大丈夫
- ※ 名前も場合によっては取得する必要がある
- ※ prefixを決めないとダメ definitions,parameters,responses,paths

