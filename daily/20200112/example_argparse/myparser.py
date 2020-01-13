import argparse
from argparse import RawTextHelpFormatter, ArgumentDefaultsHelpFormatter


# bo
class Both(argparse.RawTextHelpFormatter, argparse.ArgumentDefaultsHelpFormatter):  # bo
    # bo
    pass


# hoge
class Both2(
    # bo
    RawTextHelpFormatter,  # bo
    ArgumentDefaultsHelpFormatter,  # bo
):  # bo
    pass
