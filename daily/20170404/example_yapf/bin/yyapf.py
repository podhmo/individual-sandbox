from lib2to3.pgen2 import token
from lib2to3 import pytree
from yapf.yapflib import pytree_unwrapper as w
import yapf


def monkey_patch(cls):
    def _monkey_patch(method):
        setattr(cls, method.__name__, method)
        return method
    return _monkey_patch


@monkey_patch(w.PyTreeUnwrapper)
def Visit_import_from(self, node):
    """
    このようにimportが出力されてほしい
    from foo import (
        x,
        y,
        z,
    )
    from foo import (
        x,
    )
    from foo import (  # NOQA
        x,
        y,
    )

    このために 以下の設定を追加してこのAssignerを使う。
    - dedent_closing_brackets=true
    """
    for around_last in reversed(node.children):
        if around_last.type != token.COMMENT:
            break
    # node.type < 256は literal的なnode
    if around_last.type < 256 and around_last.value == ")":
        as_names = node.children[-2]
        if not as_names.children:
            # `from foo import (x)` to `from foo import (x,)`
            last_symbol = as_names
            parent = last_symbol.parent
            as_names = pytree.Node(298, [])
            parent.set_child(-2, as_names)
            as_names.append_child(last_symbol)
            as_names.append_child(pytree.Leaf(token.COMMA, ","))
        elif not hasattr(as_names.children[-1], "value") or as_names.children[-1].value != ",":
            # `from foo import (x, y)` to `from foo import (x, y,)`
            as_names.children.append(pytree.Leaf(token.COMMA, ","))

    self.DefaultNodeVisit(node)


@monkey_patch(w.PyTreeUnwrapper)
def Visit_import_as_names(self, node):
    """
    元の実装だと末尾にcommentがある場合に'('として扱ってくれない
    """
    prev = node.prev_sibling
    while prev.type == token.COMMENT:
        prev = prev.prev_sibling
    if prev.value == '(':
        w._DetermineMustSplitAnnotation(node)
    self.DefaultNodeVisit(node)


if __name__ == "__main__":
    yapf.run_main()
