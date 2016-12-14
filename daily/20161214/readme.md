# flask python ひどいライブラリと付き合う方法

appを引数にとってオブジェクトを作る何かについて

- appをwrapしたものならappとして扱えるので問題ない
- 都度生成して良いものならthread localにしても
- そうじゃない場合には、何か色々頑張る

[example_flask](example_flask)
