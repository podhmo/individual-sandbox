def peek(name):
    print("@", name)
    raise ImportError(name)

import sys
sys.path_hooks.insert(0, peek)
import json
