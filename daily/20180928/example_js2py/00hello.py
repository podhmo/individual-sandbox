from js2py.pyjs import *
# setting scope
var = Scope(JS_BUILTINS)
set_global_object(var)

# Code follows:
var.registers(['$'])


@Js
def PyJsHoistedNonPyName(x, this, arguments, var=var):
    var = Scope({'x': x, 'this': this, 'arguments': arguments}, var)
    var.registers(['x'])
    var.get('console').callprop('log', var.get('x'))


PyJsHoistedNonPyName.func_name = '$'
var.put('$', PyJsHoistedNonPyName)
pass
var.get('$')(Js('Hello, World!'))
pass
