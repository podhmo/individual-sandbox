import argparse

parser = argparse.ArgumentParser(prog='PROG')

group = parser.add_mutually_exclusive_group()
group.add_argument('src', nargs="?")
group.add_argument('--format', choices=["json", "yaml"])

print(parser.parse_args(['xx.json']))
print(parser.parse_args(['--format', 'json']))

# PROG: error: argument src: not allowed with argument --format
print(parser.parse_args(['--format', 'json', "yy.json"]))

