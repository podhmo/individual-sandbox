## python sqlalchemy automapで上手く行かない問題

automapの実装はmetadata.reflectした結果を利用してmapperを作成するというもの。
defaultの方法では上手く行かないものが存在する。

具体的にはこういうテーブル

```sql
CREATE TABLE person (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    father_id INTEGER,
    mother_id INTEGER,
    FOREIGN KEY(father_id) REFERENCES person(id),
    FOREIGN KEY(mother_id) REFERENCES person(id)
);
```

こちらの結果が以下の様な形になってしまう。

```json
{
  "person": {
    "father_id": {
      "type": "Integer",
      "nullable": true
    },
    "id": {
      "type": "ID",
      "nullable": false
    },
    "mother_id": {
      "type": "Integer",
      "nullable": true
    },
    "name": {
      "type": "String",
      "nullable": false
    },
    "person": {
      "table": "person",
      "direction": "MANYTOONE",
      "uselist": false,
      "relation": {
        "to": "person.id",
        "from": "person.mother_id"
      }
    },
    "person_collection": {
      "table": "person",
      "direction": "ONETOMANY",
      "uselist": true,
      "relation": {
        "to": "person.id",
        "from": "person.father_id"
      }
    }
  }
}
```

何が問題なのかというと、person,person_collectionではなくfather,motherになってほしいというところ。
(名前が適切かはともかく、father_collection,mother_collectionという形でも現れるべき)

これは name_for_scalar_relationship などの実装が以下の様になっているため。

```python
def name_for_scalar_relationship(base, local_cls, referred_cls, constraint):
    return referred_cls.__name__.lower()

def name_for_collection_relationship(
        base, local_cls, referred_cls, constraint):
    return referred_cls.__name__.lower() + "_collection"
```

class名を見ているのでそうなってしまう。
