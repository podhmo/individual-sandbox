import argparse

parser = argparse.ArgumentParser()
parser.add_argument("--xxx", choices=["xxx", "yyy", "zzz"])
print(parser.format_help())
