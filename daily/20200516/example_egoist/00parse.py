from egoist.app import parse_args

argv = ["x", "y", "z"]
print(list(parse_args(argv)))
argv = ["x", "y", "-", "z"]
print(list(parse_args(argv)))
argv = ["x", "y", "--foo", "z"]
print(list(parse_args(argv)))
argv = ["x", "y", "--foo=0", "z"]
print(list(parse_args(argv)))
argv = ["x", "y", "--foo=0", "z", "-"]
print(list(parse_args(argv)))
