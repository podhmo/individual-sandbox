from prestring.go import Module
from prestring.codeobject import Symbol
from prestring.utils import LazyFormat


def run(m: Module) -> Module:
    filename = Symbol("filename")
    doSomething = Symbol("doSomething")  # todo: import

    with m.func("Run", LazyFormat("{}: string", filename), return_="error"):
        config = load_config(m, filename)
        m.return_(doSomething(config))  # or "return nil"
    return m


def load_config(m: Module, filename: Symbol) -> Symbol:
    LoadConfig = Symbol("LoadConfig")  # todo: import
    config = Symbol("config")
    err = Symbol("err")

    m.stmt("{}, {} := {}", config, err, LoadConfig(filename))
    with m.if_(LazyFormat("{} != nil", err)):
        m.return_(err)
    return config


# ASTの変換でできないかな。。
# そしてこうなるならもう関数として括る必要もないのでは？
# やっていることは
# - "=", ":=" の使い分け (constがほしい)
def load_configX(filename: str) -> Symbol:
    from xxx import LoadConfig

    config = LoadConfig(filename)
    return config


print(run(Module()))
