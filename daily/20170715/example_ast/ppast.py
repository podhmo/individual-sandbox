import sys
import ast


def print_ast(t, indent='  ', stream=sys.stdout, initlevel=0):
    print_node(t, initlevel + 2, indent, stream.write)
    stream.write('\n')


def print_node(node, level, indent, write):
    pfx = indent * level
    pfx_prev = indent * (level - 1)
    if not isinstance(node, list):
        write(node.__class__.__name__)
    if hasattr(node, 'body'):
        if hasattr(node, 'name'):
            write(" '" + node.name + "' ")
        write('(\n')
        for child in node.body:
            write(pfx)
            print_node(child, level + 1, indent, write)
            write('\n')
        write(pfx_prev + ')')
    elif hasattr(node, 'value'):
        write('[ ')
        if hasattr(node, 'targets'):
            print_node(node.targets, level + 1, indent, write)
            write(' = ')
        print_node(node.value, level + 1, indent, write)
        if hasattr(node, 'attr'):
            write('.')
            write(node.attr)
        write(' ]')
    elif hasattr(node, 'func'):
        write('(')
        print_node(node.func, level + 1, indent, write)
        if hasattr(node, 'args'):
            write('(')
            print_node(node.args, level + 1, indent, write)
            write(')')
        write(')')
    elif isinstance(node, list):
        for n in node:
            print_node(n, level + 1, indent, write)
            write(',')
    elif hasattr(node, 'id'):
        write('(' + repr(node.id) + ')')
    elif hasattr(node, 's'):
        write('(' + repr(node.s) + ')')
