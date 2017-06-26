```
python 00write.py > 00data.tsv
python 00read.py < 00data.tsv
['foo\tbar', 'foo']
['foo"b"ar', 'bar']
["boo's something", 'boo']

python 01write.py > 01data.tsv
python 01read.py < 01data.tsv
{'event': 'foo', 'message': 'foo\tbar'}
{'event': 'bar', 'message': 'foo"b"ar'}
{'event': 'boo', 'message': "boo's something"}
```
