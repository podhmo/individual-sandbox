import jedi
import os.path


def jedi_script(source, line, column, source_path):
    return jedi.Script(source, line, column, source_path or '')


def get_definition(*args):
    definitions = jedi_script(*args).goto_definitions()
    return list(map(definition_to_dict, definitions))


def definition_to_dict(d):
    return dict(
        doc=d.docstring(),
        description=d.description,
        desc_with_module=d.desc_with_module,
        line_nr=d.line,
        column=d.column,
        module_path=d.module_path,
        name=getattr(d, 'name', []),
        full_name=getattr(d, 'full_name', []),
        type=getattr(d, 'type', []),
    )


source = """\
import kamidana
"""
print(get_definition(source, 1, 7, "."))
# (ffap-python:find-program ffap-python:python-program-name)
# activateしていないとダメ？
