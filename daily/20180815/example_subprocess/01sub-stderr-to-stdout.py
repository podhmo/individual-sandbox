import subprocess
import sys

subprocess.Popen(["python", "sub.py"], shell=False, stdout=sys.stderr).wait()
