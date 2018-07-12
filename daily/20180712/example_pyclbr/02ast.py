import contextlib
import inspect
import ast
import textwrap
import jupyter_core.application as app


def _is_sub_node(node):
    return (isinstance(node, ast.AST) and not isinstance(node, ast.expr_context))


def _is_leaf(node):
    for field in node._fields:
        attr = getattr(node, field)
        if _is_sub_node(attr):
            return False
        elif isinstance(attr, (list, tuple)):
            for val in attr:
                if _is_sub_node(val):
                    return False
    else:
        return True


def pformat(node, indent='    ', show_offsets=True, _indent=0):
    if node is None:  # pragma: no cover (py35+ unpacking in literals)
        return repr(node)
    elif _is_leaf(node):
        if show_offsets and hasattr(node, 'lineno'):
            ret = ast.dump(node)
            # For nodes like Pass() which have information but no data
            if ret.endswith('()'):
                info = '(lineno={}, col_offset={}'.format(
                    node.lineno,
                    node.col_offset,
                )
            else:
                info = '(lineno={}, col_offset={}, '.format(
                    node.lineno,
                    node.col_offset,
                )
            return ret.replace('(', info, 1)
        else:
            return ast.dump(node)
    else:

        class state:
            indent = _indent

        @contextlib.contextmanager
        def indented():
            state.indent += 1
            yield
            state.indent -= 1

        def indentstr():
            return state.indent * indent

        def _pformat(el, _indent=0):
            return pformat(
                el,
                indent=indent,
                show_offsets=show_offsets,
                _indent=_indent,
            )

        out = [type(node).__name__, '(\n']
        with indented():
            if show_offsets and hasattr(node, 'lineno'):
                fields = ('lineno', 'col_offset') + node._fields
            else:
                fields = node._fields

            for field in fields:
                attr = getattr(node, field)
                if attr == []:
                    representation = '[]'
                elif (
                    isinstance(attr, list) and len(attr) == 1 and isinstance(attr[0], ast.AST)
                    and _is_leaf(attr[0])
                ):
                    representation = '[{}]'.format(_pformat(attr[0]))
                elif isinstance(attr, list):
                    representation = '[\n'
                    with indented():
                        for el in attr:
                            representation += '{}{},\n'.format(
                                indentstr(),
                                _pformat(el, state.indent),
                            )
                    representation += indentstr() + ']'
                elif isinstance(attr, ast.AST):
                    representation = _pformat(attr, state.indent)
                else:
                    representation = repr(attr)
                out.append('{}{}={},\n'.format(indentstr(), field, representation))
        out.append(indentstr() + ')')
        return "".join(out)


method = app.Application.parse_command_line
lines, lnum = inspect.getsourcelines(method)
print(lnum)
for line in lines:
    print(line, end="")

t = ast.parse(textwrap.dedent(inspect.getsource(method)))
print(pformat(t))
