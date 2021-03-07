SELECT
    tree.value as attr_b
FROM
    json_test_table,
    json_tree(json_test_table.json, '$.attr_b') as tree -- attr_bをjson_treeとして取得する
WHERE
    tree.value IS NOT NULL
;
