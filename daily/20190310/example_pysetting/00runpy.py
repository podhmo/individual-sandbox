import runpy
import pprint

d = runpy.run_path("./settings.py")
print(type(d))
pprint.pprint({k: repr(v)[:100] for k, v in d.items()})
# -- stdout --------------------
# >> <class 'dict'>
# >> {'LOGGER': '<Logger app (INFO)>',
# >>  '__builtins__': '{\'__name__\': \'builtins\', \'__doc__\': "Built-in '
# >>                  'functions, exceptions, and other objects.\\n\\nNoteworth',
# >>  '__cached__': 'None',
# >>  '__doc__': 'None',
# >>  '__file__': "'./settings.py'",
# >>  '__loader__': 'None',
# >>  '__name__': "'<run_path>'",
# >>  '__package__': "''",
# >>  '__spec__': 'None',
# >>  'logging': "<module 'logging' from '/usr/lib/python3.7/logging/__init__.py'>",
# >>  'os': "<module 'os' from '/usr/lib/python3.7/os.py'>"}
