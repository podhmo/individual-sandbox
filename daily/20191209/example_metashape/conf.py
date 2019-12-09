from __future__ import annotations
import dataclasses


@dataclasses.dataclass
class Toplevel:
    site_name: str = "Material for MkDocs"

    class Theme:
        name: str = "material"
        language: str = "en"

        class Palette:
            primary: str = "indigo"
            accent: str = "indigo"

        palette: "Palette" = Palette

        class Font:
            text: str = "Roboto"
            code: str = "Roboto Mono"

        font: "Toplevel.Theme.Font" = Font

    theme: "Toplevel.Theme" = Theme
