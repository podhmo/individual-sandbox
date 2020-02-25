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

## flutter web

久しぶりに環境作成を
- https://flutter.dev/docs/get-started/install
- https://flutter.dev/docs/get-started/web

```
unzip ~/Downloads/flutter_macos_v1.12.13+hotfix.7-stable.zip
export PATH="$PATH:`pwd`/flutter/bin"

flutter doctor
flutter precache

flutter channel beta
flutter upgrade
flutter config --enable-web
```

## openapi docのURLをいい感じに

やりたい

