import argparse

# memo
# add_argument() -> _add_action()
# assign:
#   self._actions
#   self._option_string_actions
#   self._has_negative_number_optionals

# duplicated
parser = argparse.ArgumentParser(conflict_handler="resolve")
parser.add_argument("--xyz")
parser.add_argument("--xyz")
print(parser.parse_args(["--xyz", "foo"]))

# conflicted (overwrite)
parser = argparse.ArgumentParser(conflict_handler="resolve")
parser.add_argument("--xyz", action="store_true")
parser.add_argument("--xyz", action="store_false")
print(parser.parse_args(["--xyz"]))

# conflicted (overwrite)
parser = argparse.ArgumentParser(conflict_handler="resolve")
parser.add_argument("--xyz", action="store_true")
parser.add_argument("--xyz")
# error
# print(parser.parse_args(["--xyz"]))
print(parser.parse_args(["--xyz", "foo"]))
