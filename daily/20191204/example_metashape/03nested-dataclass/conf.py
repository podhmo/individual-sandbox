from __future__ import annotations
from magicalimport import import_module

shapes = import_module("./shapes.py", here=__file__)
toplevel = shapes.Toplevel(
    site_name="Material for MkDocs",
    site_description="A Material Design theme for MkDocs",
    site_author="Martin Donath",
    site_url="https://squidfunk.github.io/mkdocs-material/",
    # repository
    repo_name="squidfunk/mkdocs-material",
    repo_url="https://github.com/squidfunk/mkdocs-material",
    # Copyright
    copyright="Copyright &copy; 2016 - 2017 Martin Donath",
    theme=shapes.Theme(
        name="material",
        language="en",
        palette=shapes.Palette(primary="indigo", accent="indigo"),
        font=shapes.Font(text="Roboto", code="Roboto Mono"),
    ),
    extra=shapes.Extra(
        manifest="manifest.webmanifest",
        social=[
            shapes.Social(type_="github", link="https://github.com/squidfunk"),
            shapes.Social(type_="twitter", link="https://twitter.com/squidfunk"),
            shapes.Social(type_="linkedin", link="https://linkedin.com/in/squidfunk"),
        ],
    ),
)
