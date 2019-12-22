# monogusa

- https://github.com/podhmo/monogusa

## 最近のmonogusa

```
$ git ls-files ../../ | grep -ioP ".*example_monogusa/readme.md$" | sort -u | sed 's/.*/- [\0](\0)/g' | tac 
```

- [../../20191221/example_monogusa/readme.md](../../20191221/example_monogusa/readme.md)
- [../../20191218/example_monogusa/readme.md](../../20191218/example_monogusa/readme.md)
- [../../20191217/example_monogusa/readme.md](../../20191217/example_monogusa/readme.md)
- [../../20191215/example_monogusa/readme.md](../../20191215/example_monogusa/readme.md)
- [../../20191214/example_monogusa/readme.md](../../20191214/example_monogusa/readme.md)

## なにをしよう？

とりあえず昨日の段階でweb側のコード生成ができたのだった。importの部分に難があって本当はprojectの位置のようなものを固定したいかもと思ったりした。

それはそれとして今日は少しテスト辺りを整備しつつまた使用感を試す記事を書いてみても良いかもしれない。

## テストを綺麗に

少なくともcliとwebが混在していると辛いかも。
あと回帰テスト的なe2eとドキュメンテーションとしてのテストを分けたほうが良いかもしれない。

テストといいつつMakefileによるe2eなのだけど。

## そういえばDIの例(test)を用意していなかった

テキトーに追加しよう

## エラーハンドリングをやっていなかった

とりあえずstderrに出力するようにしないと。
ステータスコードのようなものをresponseで返すようにしても良いんだろうか？

エラーハンドリングはデバッグのときだけフルのスタックトレースを返すとかの方が良いんだろうか？セキュリティ的なことを考えると。まぁ良いや全部そのままstderrとして返そう。

でも200以外のstatusコードで返したいよね。。。
そう考えるとresponse_modelにstatus_codeを渡せたっけ？

- https://fastapi.tiangolo.com/tutorial/additional-responses/


## 考えてみるとopenapi docを直接見るの辛くない？

routes的なオプションを追加したくなるかも
