from lib2to3.pygram import python_symbols as syms
from lib2to3.pytree import Node, Leaf
from lib2to3.pgen2 import token
from lib2to3.fixer_util import (  # NOQA
    LParen,
    RParen,
    Assign,
    Name,
    Attr,
    Comma,
    Dot,
    Newline,
    BlankLine,
    Number,
    Subscript,
    ListComp,
    FromImport,
    ImportAndCall,
)


def maybe_name(s, prefix=" "):
    if isinstance(s, str):
        return Name(s, prefix=prefix)
    return s


def Colon():
    return Leaf(token.COLON, ":")


def Indent():
    return Leaf(token.INDENT, "    ")


def Dedent():
    return Leaf(token.DEDENT, "")


def Def(name, args, *body):
    return Node(
        syms.funcdef, [
            Name("def"), maybe_name(name), Node(syms.parameters, [
                LParen(),
                args,
                RParen(),
            ]), Colon(), Node(syms.suite, [
                Newline(),
                Indent(),
                *body,
                Dedent(),
            ])
        ]
    )


def Args(*args, **kwargs):
    r = []
    if args:
        r.append(maybe_name(args[0], prefix=""))
        for x in args[1:]:
            r.append(Comma())
            r.append(maybe_name(x))
    if kwargs:
        sorted_items = sorted(kwargs.items())
        if args:
            r.append(Comma())
            prefix = " "
        else:
            prefix = ""
        r.append(maybe_name(sorted_items[0][0], prefix=prefix))
        r.append(Leaf(token.EQUAL, "="))
        r.append(sorted_items[0][1])
        for k, v in sorted_items[1:]:
            r.append(Comma())
            r.append(maybe_name(k))
            r.append(Leaf(token.EQUAL, "="))
            r.append(v)
    return Node(syms.typedargslist, r)


def File(*nodes):
    return Node(syms.file_input, [*nodes, Leaf(token.ENDMARKER, "")])
