import tomlkit

POETRY_DEFAULT = """\
[tool.poetry]
name = ""
version = ""
description = ""
authors = []

[tool.poetry.dependencies]

[tool.poetry.dev-dependencies]
"""

content = tomlkit.loads(POETRY_DEFAULT)
poetry_content = content["tool"]["poetry"]
poetry_content["name"] = "toybox"
poetry_content["version"] = "0.0.0"
poetry_content["description"] = "Hello, this is first project, in my life."
poetry_content["authors"].append("me")
print(tomlkit.dumps(content))
