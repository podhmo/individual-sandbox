[前の](../../20161030/example_jwt)続き

headerの情報を取り出してdecodeできないかな？

```
$ python 01*
encoded: b'eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCIsInVpZCI6IjEifQ.eyJpZCI6IjEiLCJuYW1lIjoiZm9vIn0.DSEfL8u8isCmwpF2Itd8Ie9fMpSC6rYXTKr8XS3HyS0'
headers: {'alg': 'HS256', 'typ': 'JWT', 'uid': '1'}
decoded: {'id': '1', 'name': 'foo'}
```
