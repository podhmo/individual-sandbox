from prestring.text import Module
from prestring.codeobject import Symbol


def run(m: Module) -> Module:
    filename = Symbol("filename")

    m.stmt("func Run({}: string) error {{", filename)  # todo: with type
    with m.scope():
        config = load_config(m, filename)
        m.stmt("return doSomething({})", config)
        # or "return nil"
    m.stmt("}")
    return m


def load_config(m: Module, filename: Symbol) -> Symbol:
    LoadConfig = Symbol("LoadConfig")  # todo: import
    config = Symbol("config")
    err = Symbol("err")

    m.stmt("{}, {} := {}", config, err, LoadConfig(filename))
    m.stmt("if {} != nil {{", err)
    with m.scope():
        m.stmt("return err")
    m.stmt("}")
    return config


print(run(Module(indent="\t")))
