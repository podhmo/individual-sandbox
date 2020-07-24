## やりたいこと

- structscan
- egoist
- metashape
- monogusa
- handofcats
- (swagger-marshmallow-codegen, alchemy-jsonschema)

この辺親しい感じのあれ

利便性のために

- dictknife
- kamidana
- magicalimport
- (miniconfig)
- (minitask)

### 気にしていること

- namedtypeをどう扱うかをもう少し丁寧に整理したほうが良いかも？
- 既存のトップレベルの値をnamedとして扱うのはあんまり良くない
- NewType対応はどうしよう？

## mypy

pluginの作り方どうするんだっけ？

- https://mypy.readthedocs.io/en/stable/extending_mypy.html

pluginのインターフェイスを気にしないとか

```
mypy.plugin:ChainedPlugin <- mypy.plugin:Plugin <- mypy.plugin:CommonPluginApi <- builtins:object
    [method, OVERRIDE] __getstate__
    [method, OVERRIDE] __init__(self, /, *args, **kwargs)
    [method, OVERRIDE] __mypyc_defaults_setup
    [static method, OVERRIDE] __new__(*args, **kwargs)
    [method, OVERRIDE] __setstate__
    [method] _find_hook
    [method, OVERRIDE] get_additional_deps
    [method, OVERRIDE] get_attribute_hook
    [method, OVERRIDE] get_base_class_hook
    [method, OVERRIDE] get_class_decorator_hook
    [method, OVERRIDE] get_customize_class_mro_hook
    [method, OVERRIDE] get_dynamic_class_hook
    [method, OVERRIDE] get_function_hook
    [method, OVERRIDE] get_metaclass_hook
    [method, OVERRIDE] get_method_hook
    [method, OVERRIDE] get_method_signature_hook
    [method, OVERRIDE] get_type_analyze_hook
    [method, OVERRIDE] report_config_data
    [method, OVERRIDE] set_modules

mypy.plugin:Plugin <- mypy.plugin:CommonPluginApi <- builtins:object
    [method, OVERRIDE] __getstate__
    [method, OVERRIDE] __init__(self, /, *args, **kwargs)
    [method, OVERRIDE] __mypyc_defaults_setup
    [static method, OVERRIDE] __new__(*args, **kwargs)
    [method, OVERRIDE] __setstate__
    [method] get_additional_deps
    [method] get_attribute_hook
    [method] get_base_class_hook
    [method] get_class_decorator_hook
    [method] get_customize_class_mro_hook
    [method] get_dynamic_class_hook
    [method] get_function_hook
    [method] get_metaclass_hook
    [method] get_method_hook
    [method] get_method_signature_hook
    [method] get_type_analyze_hook
    [method, OVERRIDE] lookup_fully_qualified
    [method] report_config_data
    [method] set_modules

mypy.plugin:CommonPluginApi <- builtins:object
    [method] __getstate__
    [method] __mypyc_defaults_setup
    [static method, OVERRIDE] __new__(*args, **kwargs)
    [method] __setstate__
    [method] lookup_fully_qualified
```

### 新しい型を作ったところでhookしたい

そういえば、こういうエラーが出るのでこの辺りから調べていこう。

```
Direction = NewType("Direction", Literal["N", "S", "W", "E"])
# error: Argument 2 to NewType(...) must be subclassable
```

- https://github.com/python/mypy/blob/master/mypy/semanal_newtype.py#L35
- https://github.com/python/mypy/blob/master/mypy/semanal.py#L1898

ast的な部分ってどうやって手に入れているんあろう？

```
ctx.call
# DynamicClassDefContext(call=<mypy.nodes.CallExpr object at 0x10eef1ba0>, name='XXX', api=<mypy.semanal.SemanticAnalyzer object at 0x10ec5fdc0>)

<mypy.nodes.CallExpr object at 0x10eef1ba0>

ctx.call.args[1].base.name
'Literal'
```

呼び出しているのはこの辺。

- https://github.com/python/mypy/blob/master/mypy/semanal.py#L2194

このメッセージを殺さないと

```
00x.py:8: error: Variable "00x.XXX" is not valid as a type
00x.py:8: note: See https://mypy.readthedocs.io/en/latest/common_issues.html#variables-vs-type-aliases
Found 1 error in 1 file (checked 1 source file)
```

この辺で起きている。 `isinstance(sym.node, Var):` がだめ。

- https://github.com/python/mypy/blob/28829fbb684d7535680934b401c05698db5b4d85/mypy/typeanal.py#L427-L430
- TypeInfoになれば良い？

```console
$ grep SymbolNode nodes.py | grep "^class"
class SymbolNode(Node):
class MypyFile(SymbolNode):
class ImportedName(SymbolNode):
class OverloadedFuncDef(FuncBase, SymbolNode, Statement):
class FuncDef(FuncItem, SymbolNode, Statement):
class Decorator(SymbolNode, Statement):
class Var(SymbolNode):
class TypeVarExpr(SymbolNode, Expression):
class TypeInfo(SymbolNode):
class TypeAlias(SymbolNode):
class PlaceholderNode(SymbolNode):
```

この順序なので、間に合ってないかも。
```py

        s.is_final_def = self.unwrap_final(s)
        self.analyze_lvalues(s)
        self.check_final_implicit_def(s)
        self.check_classvar(s)
        self.process_type_annotation(s)
        self.apply_dynamic_class_hook(s)
```

### mainからみよう

`mypy.__main__:main`

いろいろ見ていくとこの辺りがmain

- https://github.com/python/mypy/blob/master/mypy/build.py#L136

## python metashape

細かい調整をしてみる？

- Annotatedの部分のtyping error
- nested class等に対応

## teraform-cdk

そういえば、そういうはなしがあったのだった。

- https://github.com/hashicorp/terraform-cdk/tree/master/examples/python-aws

## hmm

- https://qiita.com/Saku731/items/741fcf0f40dd989ee4f8
- https://qiita.com/qnoyxu/items/8b07d571ef858608e93d

## hmm

- UserListとかをwalk()
- simpli `def_` -> `def`
