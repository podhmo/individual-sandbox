# mongoengineを試す

- 異なる形状の値が返ってきたらエラーくらいにはなって欲しい

- [1. Tutorial — MongoEngine 0.10.6 documentation](http://docs.mongoengine.org/tutorial.html)
- [リストを保持する方法](2.3. Defining documents — MongoEngine 0.10.6 documentation
http://docs.mongoengine.org/guide/defining-documents.html#one-to-many-with-listfields)
- [fields](http://docs.mongoengine.org/guide/defining-documents.html#fields)

upsertについて

- [python - Mongoengine update_one+upsert vs. deprecated get_or_create - Stack Overflow](http://stackoverflow.com/questions/24738617/mongoengine-update-oneupsert-vs-deprecated-get-or-create)
- [django - MongoEngine: Replacing get_or_create with upsert/update_one - Stack Overflow](http://stackoverflow.com/questions/25846462/mongoengine-replacing-get-or-create-with-upsert-update-one)

memo: 以下がわからない

- EmbeddedDocument

リストを保持する方法

```python
class User(Document):
    name = StringField()

class Page(Document):
    content = StringField()
    authors = ListField(ReferenceField(User))
```

単なる文字列の場合には？

```python
class Page(Document):
    content = StringField()
    tags = ListField(StringField())
```


# swagger

- [Swagger Editor – Swagger](http://swagger.io/swagger-editor/)

## swaggerを手元で動かす

- swagger editor
- swagger ui

### swagger editor

こちらはなくて良いのでは？

### swagger ui

```
$ cd example_swagger
$ git clone --depth=1 git@github.com:swagger-api/swagger-ui.git
$ docker-compose build
$ docker-compose up ui
```

```
$ open http://localhost:4444
# 以下などを指定する
https://gist.githubusercontent.com/podhmo/e045502fd42311376b6f2ee050b79168/raw/52c962a74bbbb479b7e2b3b2a151e3adacc16d15/swagger.yaml
```

## docker-compose reference

- [Docker Compose - docker-compose.yml リファレンス - Qiita](http://qiita.com/zembutsu/items/9e9d80e05e36e882caaa)
