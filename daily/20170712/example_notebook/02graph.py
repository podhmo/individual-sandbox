import textwrap
from nbformat.v4 import new_code_cell, new_notebook, writes_json

notebook = new_notebook()
sources = [
    """
    import random
    random.random()
    """,
    """
    import random
    random.random()
    """,
]

for i, source in enumerate(sources, 1):
    notebook["cells"].append(new_code_cell(textwrap.dedent(source), execution_count=i))

print(writes_json(notebook))
