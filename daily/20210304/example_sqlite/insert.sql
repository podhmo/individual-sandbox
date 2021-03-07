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
