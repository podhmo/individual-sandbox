# sqlite3 json field

- https://www.sqlite.org/json1.html
- https://qiita.com/smith/items/c0bd002666a23a6fe8e4

```console

$ cat insert.sql | sqlite3 -echo data.db
CREATE TABLE json_test_table (
    id   INTEGER PRIMARY KEY AUTOINCREMENT,
    name VARCHAR UNIQUE,
    json JSON -- NEW!
);

INSERT INTO json_test_table (
    name,
    json
)
VALUES (
    'test_name_1',
    json(json_object( -- objectは{}で囲うオブジェクト型のこと
        'key_for_int', 1,
        'key_for_str', 'test_str',
        'key_for_arr', json('[1, "a", 2, "b"]'), -- json()で囲わないと文字列として保存される
        'key_for_obj', json('{"c":3, "d":4}'),
        'attr_a',      'nice_attr'
    ))
);

INSERT INTO json_test_table (name, json) -- json()は文字列も評価できる
VALUES (
    'test_name_2',
    json('{
        "key_for_int":2,
        "key_for_str":"test_str2",
        "key_for_arr":[5, "e", 6, "f"],
        "key_for_obj":{"g":7, "h":8},
        "attr_b":"good_attr"
    }')
);

$ cat 00query.sql | sqlite3 -echo data.db
SELECT 
    json_extract(json, '$.key_for_int')    as json_int, -- ルートを$として、jsのアクセサの記法で値を取得する
    json_extract(json, '$.key_for_str')    as json_str,
    json_extract(json, '$.key_for_arr[1]') as json_array_element_2,
    json_extract(json, '$.key_for_obj.d')  as json_array_object_d
FROM
    json_test_table
WHERE
    name = 'test_name_1'
;
            json_int = 1
            json_str = test_str
json_array_element_2 = a
 json_array_object_d = 4

$ cat 01query.sql | sqlite3 -echo data.db
SELECT
    tree.value as attr_b
FROM
    json_test_table,
    json_tree(json_test_table.json, '$.attr_b') as tree -- attr_bをjson_treeとして取得する
WHERE
    tree.value IS NOT NULL
;
attr_b = good_attr

$ cat 02query.sql | sqlite3 -echo data.db
SELECT
    json_extract(json_test_table.json, '$.attr_b') as attr_b
FROM
    json_test_table
WHERE
    json_extract(json_test_table.json, '$.attr_b') IS NOT NULL
;
attr_b = good_attr

$ cat 03query.sql | sqlite3 -echo data.db
SELECT
    tree.json
FROM
    json_test_table,
    json_tree(json_test_table.json) as tree
WHERE
    name = 'test_name_1'
;
 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

 json = {"key_for_int":1,"key_for_str":"test_str","key_for_arr":[1,"a",2,"b"],"key_for_obj":{"c":3,"d":4},"attr_a":"nice_attr"}

$ cat 04query.sql | sqlite3 -echo data.db
SELECT
    name
FROM
    json_test_table
ORDER BY
    json_extract(json, '$.key_for_str') DESC
;
 name = test_name_2

 name = test_name_1
```
