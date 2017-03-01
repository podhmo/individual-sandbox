# python pep487のこと

参考

- https://docs.python.org/3/whatsnew/3.6.html#pep-487-simpler-customization-of-class-creation
- https://github.com/tecki/metaclasses
- https://www.python.org/dev/peps/pep-0487/

出来る範囲はmetaclassの__init__でできることまで

- __new__ classを生成するhook
- __init__ class 生成後のhook
- __call__ instance生成時のhook -> abcで使われている。使われていない。typobject.c見るとPyBaseObject_Typeのobject_new。PyType_Type

objectの__new__はimmutable objectのあれこれ。

# golang emacs flycheck中のerrcheckとunconvertでCPUを使い尽くす

外したい。

```lisp
  (eval-after-load 'flycheck
    '(progn
       ;; (flycheck-checker-get 'go-gofmt 'next-checkers)
       (setf (get 'go-gofmt (flycheck--checker-property-name 'next-checkers))
             '((warning . go-golint)
               ;; Fall back, if go-golint doesn't exist
               (warning . go-vet)
               ;; Fall back, if go-vet doesn't exist
               (warning . go-build) (warning . go-test)
               ; (warning . go-errcheck)
               ; (warning . go-unconvert)
               )

             )
       ))
```

# python dictknifeを修正していた

adhocに名前を決めちゃいたい。

- 名前がない場合 "Error.yaml" -> ファイル名から
- definitions/xxxx みたいなやつ -> そのまま
- /xxxx みたいなやつ -> prefixを補う
- 即時定義 -> flatになった段階で大丈夫
- ※ 名前も場合によっては取得する必要がある
- ※ prefixを決めないとダメ definitions,parameters,responses,paths

