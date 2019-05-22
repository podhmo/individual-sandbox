import subprocess

input_text = """
aaa
bbb
ccc
""".strip()

subprocess.run(["sed", "s/^/@@/g"], input=input_text, text=True)
