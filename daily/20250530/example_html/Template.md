# 一般的なタスク

## テキスト要約
指定されたテキストを簡潔に要約します。
````
以下のテキストを3つの主要なポイントで要約してください：

{{text_to_summarize}}
````

## アイデア生成
特定のテーマに関する新しいアイデアを複数提案します。
````
テーマ： {{theme}}

このテーマに基づいて、革新的な製品またはサービスのアイデアを5つ提案してください。各アイデアには、簡単な説明とターゲット顧客を含めてください。
````

## メール作成
指定された目的のメール文面を作成します。
````
宛先： {{recipient_name}} 様
件名： {{subject}}

目的：
{{mail_purpose}}

上記の目的を達成するための丁寧なメールを作成してください。差出人は「{{sender_name}}」とします。
````

# コーディング支援

## Python関数スターター
指定された機能を持つPython関数の雛形を生成します。
````python
# 機能: {{function_description}}
# 入力例: {{input_example}}
# 出力例: {{output_example}}

def {{function_name}}({{parameters}}):
    """
    {{function_docstring}}
    """
    # ここに処理を記述してください
    pass

# テストコード (任意)
# print({{function_name}}(...))
````

## SQLクエリ生成
指定された条件に合うSQLクエリを生成します。
````sql
-- テーブル名: {{table_name}}
-- 取得したいカラム: {{columns_to_select}}
-- 条件: {{where_condition}}
-- 並び順: {{order_by_clause}}

SELECT {{columns_to_select}}
FROM {{table_name}}
WHERE {{where_condition}}
ORDER BY {{order_by_clause}};
````

# 学習・教育

## 概念説明
複雑な概念を分かりやすく説明します。
````
概念： {{concept_name}}
対象読者： {{target_audience}} (例: 中学生、プログラマー未経験者など)

上記の概念を、対象読者が理解できるように、具体例やアナロジーを交えて説明してください。
````
