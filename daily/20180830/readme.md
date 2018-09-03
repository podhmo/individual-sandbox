## python unescape json

こういう文字列がたまにある

```
"{\"name\": \"foo\", \"age\": 20}"
```

どうすれば良いのかなーと言う話。
こう？

```python
s.encode("utf-8").decode("unicode-escape").strip('"\n ')
```

ちなみに

```python
>>> '\\u3042'.encode().decode("unicode-escape")
'あ'
```

ちなみに

```
>>> "あ".encode("ascii", "xmlcharrefreplace")
b'&#12354;'
>>> "あ".encode("ascii", "backslashreplace")
b'\\u3042'
>>> "あ".encode("ascii", "namereplace")
b'\\N{HIRAGANA LETTER A}'
```

```
>>> import html
>>> html.unescape('&#12354;')
'あ'
```


- https://docs.python.jp/3/howto/unicode.html
- https://docs.python.jp/3/library/codecs.html#error-handlers
- https://docs.python.org/3/library/html.html
