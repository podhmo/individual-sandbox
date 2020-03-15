## python 良い感じのASTを使ったhandling

色々といじれるようになった。

- 結局astのノードベースの対応にしてみた
- (意外とDict,List,...付近の対応も必要でだるい）
- optional的な機構を入れられたのはなるほどと思った
- bop,uopの他にmopがほしいかもしれない
- すべてをQというオブジェクトで包むのがちょっとあれ

## python detector

- optionalの対応をしたことでとりあえず繋げられそうという段階になった
- annotationsという試みがほしくなったし、だいぶjson2swaggerのrefineっぽい感じになっている
- 一旦pythonのtypeのことは後回しにして、graphqlの方を考える(graphql-core-next)
- unionをどうしようかな～というのは悩みどころ
- 結局型部分の生成が悩みどころなのでは？

## python query graphql like

- 考えてみるとqueryではなくselect

## go additionalProperties:true

良い方法が見つからないな

### ついでにpythonでも

- marshmallowでならむりやりできる
- 既存のものだけで十分かと思ったらdump時にちょっと困るのか
- pydanticではloadの方法がわからない


