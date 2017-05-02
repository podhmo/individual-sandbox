import argparse

parser = argparse.ArgumentParser("cmd")
parser.add_argument("--without-debug", action="store_false", dest="debug")
parser.add_argument("--with-debug", action="store_true", dest="debug")
parser.set_defaults(debug=False)
args = parser.parse_args()

print(args)
