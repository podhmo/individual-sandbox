from magicalimport import import_module
from metashape.runtime import get_walker

models = import_module("./models.py", here=__file__)
w = get_walker(models.Order, aggressive=True, recursive=True)
for cls in w.walk():
    print(cls.__name__)
from metashape.outputs.openapi import emit
import sys

w = get_walker(models.Order, aggressive=True, recursive=True)
emit(w, output=sys.stdout)
