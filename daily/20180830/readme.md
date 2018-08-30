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
