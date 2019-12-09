from __future__ import annotations
import typing as t
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

        palette: t.Type["Toplevel.Theme.Palette"] = Palette

        class Font:
            text: str = "Roboto"
            code: str = "Roboto Mono"

        font: t.Type["Toplevel.Theme.Font"] = Font

    theme: t.Type["Toplevel.Theme"] = Theme

# hmm
#
# diff --git a/metashape/analyze/typeinfo.py b/metashape/analyze/typeinfo.py
# index ede8971..9074fc5 100644
# --- a/metashape/analyze/typeinfo.py
# +++ b/metashape/analyze/typeinfo.py
# @@ -196,6 +196,8 @@ def typeinfo(
#                      is_optional=is_optional,
#                  )
#              else:
# +                inner = typing_inspect.get_args(typ)[0]
# +                return typeinfo(inner)
#                  raise ValueError(f"unsuported type %{typ}")
 
#      supertypes = []
