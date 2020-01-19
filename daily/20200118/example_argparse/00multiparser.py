import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--foo")
args, rest = parser.parse_known_args()
print("**END**")
