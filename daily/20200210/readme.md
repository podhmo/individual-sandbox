## python typed boto

何か良い方法ない？

- botocore/dataのなかにJSONが入っている
- botocoreのjsonのschemaは以下の様な感じ

  - pathsではなくoperations
  - definitionsではなくshapes
  - propertiesではなくmembers
  - type; objectではなくstructure
  - http. methodの辺は謎。

### hmm

- objectを引数定義に
- objectをtypeddictに
- 全体をmethodに
- moduleを分割して
- (mypyのときだけ全部読む？)

### そういえば

monogusaのと起動してたっけ？

- 関数の引数をobjectにするものか。あれは。


## python aws

- receiveのときにgroupIDは見れないのだっけ？そうっぽい
