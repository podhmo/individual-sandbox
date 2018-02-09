## python 何か雑なhash値(ハッシュ値じゃない。乱数(乱数じゃない))

```
python -c 'import random; import string; print("".join(random.choices(string.hexdigits, k=7)))'
python -c 'import uuid; print(uuid.uuid4().hex)'
```
