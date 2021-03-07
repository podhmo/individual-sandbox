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
