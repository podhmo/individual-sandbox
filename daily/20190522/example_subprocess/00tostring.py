import subprocess

p = subprocess.run(["ls"], stdout=subprocess.PIPE, text=True)
print(p.stdout.strip())
