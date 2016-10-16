# python pymongo + 制限されたdict

[前回](../20161015/readme.md) からの進捗

## 進捗2

collections.UserListが使えないことがわかった。
詳しくは[ここ](../20161015/example_pymongo/03about_extend_encoder.md)

まじめにC拡張いじらないとダメ。

## 進捗3

- [mongodb/mongo-python-driver: PyMongo - the Python driver for MongoDB](https://github.com/mongodb/mongo-python-driver)

forkしてcollections.Sequenceを特別扱いするようにした。久しぶりにC書いたし。GOTOを使ったので悪。
