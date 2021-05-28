## python dataclassesの欠点

- 継承を使ったときのrequired,unrequired fieldの関係がだるい
- `__slots__` の対応
- ネストした構造はjson.loads()とかで一発で変換できない
- validatioinが無い

