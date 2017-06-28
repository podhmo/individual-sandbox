## python graphql

pythonでgraphqlをやるにはだいたい以下のことを知っていれば良い

- graphql-core
- graphene

後者が概ね抽象レイヤー

db無しバージョンなどはgrapeneのexampleを見れば良い。

### example

flaskを使ったexampleはこのあたり

- https://github.com/graphql-python/graphene-sqlalchemy/tree/master/examples/flask_sqlalchemy

`:5000/graphql` にアクセスする。

```
{
  allEmployees {
    edges {
      node {
        id
        name
        hiredOn
        role {
          id
          name
        }
        department {
          id
          name
        }
      }
    }
  }
}
```


## github 表

- https://gist.github.com/mignonstyle/083c9e1651d7734f84c99b8cf49d57fa#table-%E8%A1%A8

表書けたっぽい

|A|B|
|:-|:-|
|1|2|

## xargs unix

xargsで指定すると並列実行してくれる。良い。
