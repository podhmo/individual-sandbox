# TODO: このファイルを消し去りたい
from magicalimport import import_module
from monogusa.cli.runtime import create_parser

commands = import_module("./app/commands.py", here=__file__)
di = import_module("./app/di.py", here=__file__)

parser = create_parser(commands)
args = parser.parse_args()

print(args)

params = vars(args).copy()
fn = params.pop("subcommand")
print(fn, di.resolve_args(fn))

fn(*di.resolve_args(fn), **params)
