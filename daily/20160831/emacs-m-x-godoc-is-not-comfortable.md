# golang emacs go-mode.elについて

## M-x godocが使いにくい理由

- 開くquery毎に異なるbufferを開く -> bufferが散らかるので辛い
- godoc -> completing readにはなるがpackage名の入力が面倒
- (以前開いたgodocの履歴が無いので辛い？)


## godoc-at-pointが使いにくい理由

- godoc-at-pointにキーバインドが設定されていない
- godoc-at-pointの後に戻るのがめんどくさい(godoc-and-godef使っている場合)

## hmm

- godocでcompleting-readは有効になっているけれど。やっぱりanything/helmのinterfaceが欲しい。
