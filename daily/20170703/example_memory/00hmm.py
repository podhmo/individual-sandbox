import types


def print_varsize():
    print("{}{: >15}{}{: >10}{}".format('|', 'Variable Name', '|', '  Size', '|'))
    print(" -------------------------- ")
    for v in globals():
        if 'size' in dir(eval(v)) and not v.startswith('_') and not isinstance(
            eval(v), types.ModuleType
        ):
            print("{}{: >15}{}{: >10}{}".format('|', v, '|', str(eval(v).size), '|'))
        elif '__len__' in dir(eval(v)) and not v.startswith('_') and not isinstance(
            eval(v), types.ModuleType
        ):
            print("{}{: >15}{}{: >10}{}".format('|', v, '|', str(len(eval(v))), '|'))


L0 = [10, 20, 30]
L1 = [10, 20, 30, 40, 50]
print_varsize()
