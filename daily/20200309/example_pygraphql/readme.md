# json2graphql

[../../20200305/example_pygraphql](../../20200305/example_pygraphql)の続き

## graphqlの型って何があったっけ？

<!-- (add-to-list 'exec-path (file-name-directory (ffap-python:find-python))) -->

https://graphql.org/learn/schema/#scalar-types

| type | description |
| :--- | ---: |
| Int | A signed 32‐bit integer. |
| Float | A signed double-precision floating-point value. |
| String | A UTF‐8 character sequence. |
| Boolean | False |
| ID | The ID scalar type represents a unique identifier, often used to refetch an object or as the key for a cache. The ID type is serialized in the same way as a String; however, defining it as an ID signifies that it is not intended to be human‐readable.|

json2pythonの流れで作れば良くない？
