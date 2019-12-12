from __future__ import annotations
from typing import List
import dataclasses
from metashape.constants import ORIGINAL_NAME


@dataclasses.dataclass
class Social:
    type_: str = dataclasses.field(metadata={ORIGINAL_NAME: "type"})
    link: str


class Toplevel:
    site_name: str = "Material for MkDocs"
    site_description: str = "A Material Design theme for MkDocs"
    site_author: str = "Martin Donath"
    site_url: str = "https://squidfunk.github.io/mkdocs-material/"

    # repository
    repo_name: str = "squidfunk/mkdocs-material"
    repo_url: str = "https://github.com/squidfunk/mkdocs-material"

    # Copyright
    copyright: str = "Copyright &copy; 2016 - 2017 Martin Donath"

    class theme:
        name: str = "material"
        language: str = "en"

        class palette:
            primary: str = "indigo"
            accent: str = "indigo"

        class font:
            text: str = "Roboto"
            code: str = "Roboto Mono"

    class extra:
        manifest: str = "manifest.webmanifest"
        social: List[Social] = [
            Social(type_="github", link="https://github.com/squidfunk"),
            Social(type_="twitter", link="https://twitter.com/squidfunk"),
            Social(type_="linkedin", link="https://linkedin.com/in/squidfunk"),
        ]
