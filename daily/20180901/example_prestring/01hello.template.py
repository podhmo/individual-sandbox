from prestring.python import Module
m = Module()  # noqa
m.sep()


with m.def_('hello', 'name', '*', 'message: str =  "hello world"'):
    m.docstring('greeting message')
    m.stmt('print(f"{name}: {message}")')


print(m)
