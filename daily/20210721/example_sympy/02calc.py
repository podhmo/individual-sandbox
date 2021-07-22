from typing import Any, Optional
from sympy import parse_expr, pi, Symbol, E


def calc(expr_str: str, *, vars: Optional[dict[str, any]] = None) -> Any:
    if vars is None:
        vars = {}

    # evaluate=Trueだとeval()を使ってしまうっぽい
    # ただし、 When evaluate=False, some automatic simplifications will not occur:
    #
    # 追記: 嘘。どちらにしろ呼ばれる。 https://github.com/sympy/sympy/blob/fb2d299937fbfd608b61d368466e4b914f96c861/sympy/parsing/sympy_parser.py#L1017
    # evaluateのTrue/Falseに関わらずevalが呼ばれてしまう。evaluate=Falseの時にはcompileして実行
    expr = parse_expr(expr_str, local_dict={"π": pi, "e": E}, evaluate=False)

    syms = {Symbol(name): val for name, val in vars.items()}
    return expr.evalf(subs=syms)


print(calc("(e**pi + 1) * cos(2/7 * π) + ln(3)"))
print(calc("x * ((e**pi + 1) * cos(2/7 * π) + ln(3))", vars={"x": 100}))