import jinja2

t = jinja2.Template(
    """
{% macro sq(x) %}  {{x * x}}  {% endmacro %}
"""
)
m = t.module
print(m.sq)
print(m.sq(10))
