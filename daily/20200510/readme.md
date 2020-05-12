## egoist, metashape

- typeinfoを書き換えていた

  - fronzen=True化できたが、setなどに入れた時に遅くなるような気がする。
  - dataclassesのhash=falseなどを追加すると良いかもしれない
  - 結局、一つの巨大なdataclassesを作ることにした
  - 欲しい情報もう少しあるかもしれない

    - enum?
    - new type?

- nestしたpointerに対応する
- optionalを取り除きたい
- newTypeの__name__なし
