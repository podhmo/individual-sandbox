# 複雑な形変数を持った型定義を複数の関数間で共有したい

実はあんまりわかっていない気がする。
というわけで、parseArgs, buildHelpの２つで共有してみる

## shared-types.ts

それっぽい定義は書けるが不満が色々ある。requiredに意図しない値が渡せる。

## shared-type2.ts

requiredに意図しない値が渡せないようにするには as constのアサーションを忘れずに書かなくてはいけない。

chatGPTに問いかけまくって答えが出た。

https://chatgpt.com/share/67222f83-703c-8001-9dda-f4355efa7284

## shared-type3.ts

as constを忘れた場合にエラーにすることはできた。ただしrequiredで上手く縛ることができない。

## shared-type4.ts

requiredで縛ることができた。型変数を増やさないとAかBのunionであればOKなどになってしまったり、増やしてもstringにwindingされてしまっていた。

`Options<any,any,any>` は無くせるんだろうか？
あと可能なら `parseArgs()` の戻り値の型がvscodeなどで見たときに直接リテラルみたいな感じで表示されると嬉しい。

## shared-type5.ts

雑にcopilotに名前の付け方をいい感じにしてくれるにお願いした。これはあまり嬉しくない。

## shared-type6.ts

`Options<any, any, any>` のanyをなくす方法を聞いたところ、Optionsの定義を消して直接記述することを勧められた。たしかに結局この方が見やすい気がする。
あと、考えてみるとbuildHelp()は別に制約を設けなくて良い気もした。

https://chatgpt.com/share/67229794-57f8-8001-81d6-134d7c9870ec

## shared-type7.ts

defaultに対応してみる。defaultに制約をかけることはできたが、defaultに渡されたオブジェクトのkeyofを取り出してそれに合致したものはundefinedの可能性をなくすということができなかった。
