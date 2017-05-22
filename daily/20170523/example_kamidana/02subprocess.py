import subprocess

cmd = """
python -c 'import sys; sys.stderr.write("<<<<<\\n")'
ls
python -c 'import sys; sys.stderr.write(">>>>>\\n")'
""".strip().replace("\n", " && ")
x = subprocess.run(cmd, shell=True, check=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
print("@@")
print(x.stdout.decode("utf-8"))
print("@@")
