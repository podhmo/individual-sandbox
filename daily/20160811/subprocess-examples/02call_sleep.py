import subprocess
from subprocess import PIPE
import re


po = subprocess.run("sleep 2 && ls -l | grep -o '[^ ]\.*py'", shell=True, check=True, stdout=PIPE)

print("-- output --------------------------------------")
print(re.sub("(?:^)|(\n)", "\\1\t", po.stdout.decode("utf-8").rstrip()))
print("----------------------------------------")
