import sys
import jinja2


loader = jinja2.DictLoader(
    {
        "x.jinja2": """
    # access undefined
    
    {{xxx}} # <- accessing undefined variable, here.
    """
    }
)
# code = jinja2.Environment().compile(t, raw=True)
# print(code)
jinja2.Environment(loader=loader).compile_templates(
    "x.jinja2", zip=None
)
