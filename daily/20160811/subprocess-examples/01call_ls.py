import subprocess
from subprocess import PIPE


po = subprocess.run("ls -l", shell=True, check=True, stdout=PIPE)

print("-- output --------------------------------------")
print(po.stdout.decode("utf-8").rstrip())
print("----------------------------------------")
