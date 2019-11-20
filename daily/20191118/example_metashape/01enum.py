from jinja2 import Template

source = """
definitions:
  {{ en_name }}:
    description: {{ ja_name }}の種類
    type: string
    enum:
    {%- for c in categories %}
      - {{ c.en_name }}
    {%- endfor %}
    x-ja-enum:
    {%- for c in categories %}
      - {{ c.ja_name }}
    {%- endfor %}

parameters:
  {{ en_name }}:
    name: {{ en_name }}
    in: query
    description: {{ ja_name }}の種類
    type: string
    enum:
    {%- for c in categories %}
      - {{ c.en_name }}
    {%- endfor %}
    x-ja-enum:
    {%- for c in categories %}
      - {{ c.ja_name }}
    {%- endfor %}
"""
categories = [
    {"ja_name": "春", "en_name": "spring"},
    {"ja_name": "夏", "en_name": "summer"},
    {"ja_name": "秋", "en_name": "autumn"},
    {"ja_name": "冬", "en_name": "winter"},
]
print(Template(source).render(en_name="season", ja_name="四季", categories=categories))
