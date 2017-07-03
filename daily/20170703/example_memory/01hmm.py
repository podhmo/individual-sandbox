import sys
import types


def print_varsize():
    print("{}{: >15}{}{: >10}{}".format('|', 'Variable Name', '|', '  Size', '|'))
    print(" -------------------------- ")
    for k, v in globals().items():
        if not k.startswith("_"):
            if hasattr(v, "size") and not isinstance(v, types.ModuleType):
                print("|{: >15}|{: >10}|".format(k, str(k.size)))
            elif hasattr(v, "__len__") and not isinstance(v, types.ModuleType):
                print("|{: >15}|{: >10}|".format(k, str(sys.getsizeof(v))))


L0 = [10, 20, 30]
L1 = [10, 20, 30, 40, 50]
L2 = [object()] * 100
L3 = [object() for i in range(100)]
print_varsize()
