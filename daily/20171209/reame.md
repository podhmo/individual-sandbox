## python もう少しstrictなかんじに

- specだけではなく引数のsignatureも
- `__enter__()`を強制する
- エラーになっても閉じられるやつ

### 例外をにぎりつぶして終了処理をする

- contextlibのExitStackをつかう
- 自作でcontextlib.contextmanagerのwrapperを書く
- 関数の中でtry-finallyをする

@:ExitStackの方は利用者側の問題かも
[昔に書いていた](http://pod.hatenablog.com/entry/2017/07/22/213803)

### `__enter__()`を強制する

