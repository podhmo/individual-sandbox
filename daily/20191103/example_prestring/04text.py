from prestring._cli import main_transform
from prestring.python import Module as PyModule
from prestring.text import Module
from prestring.text.transform import transform_file
main_transform(Module=PyModule, OutModule=Module, transform_file=transform_file)
