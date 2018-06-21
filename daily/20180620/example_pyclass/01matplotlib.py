import inspect
from shape import parse, emit
import matplotlib.backends.backend_svg as m

for name, val in m.__dict__.items():
    if inspect.isclass(val) and m.__name__ == val.__module__:
        print("----------------------------------------")
        print(emit(parse(val)))
