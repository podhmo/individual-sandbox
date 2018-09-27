import js2py
import sys
from handofcats import as_command


@as_command
def run(src: str) -> None:
    with open(src) as rf:
        source = rf.read()
    print(js2py.translate_js(source))

    # Redefine builtin objects... Do you have a better idea?
    if "js2py" in sys.modules:
        del sys.modules["js2py"]

    # del js2py.pyjs
    # del js2py
    from js2py.pyjs import Scope, JS_BUILTINS, set_global_object, Js
    # setting scope
    var = Scope(JS_BUILTINS)
    set_global_object(var)
    # Code follows:
    var.registers([u'$'])
    var.put(u'$', Js(5.0))
