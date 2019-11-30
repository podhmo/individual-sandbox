import argparse

parser = argparse.ArgumentParser()
parser.print_usage = parser.print_help  # hack
parser.parse_args()
