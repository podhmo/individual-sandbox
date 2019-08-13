## テキトウなmajor modeの作成

だいたいこういう感じ

- # がコメント
- @ を含む行があったらエラー
- [0-9]+がテキトウにhighlight (数名扱い)
- defineという予約語

例

```
# this is comment
(define x = 10)
(define y = 20)
(define z = x + y)

(define x = 10 ; define y = 20)
```
