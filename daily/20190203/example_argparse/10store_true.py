import argparse
parser = argparse.ArgumentParser()
parser.add_argument("--fullscan", action="store_true", help="full scan for guessing headers")
print(parser.parse_args([]))
