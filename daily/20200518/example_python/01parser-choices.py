import argparse

parser = argparse.ArgumentParser()
choices = ["xxx", "yyy"]
parser.add_argument("--xxx", choices=choices)
choices.append("zzz")
print(parser.format_help())
