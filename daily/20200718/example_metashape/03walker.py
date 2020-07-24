from metashape import runtime
from metashape.outputs.openapi import codegen


class User:
    name: str
    def_: str


codegen(runtime.get_walker(User))
