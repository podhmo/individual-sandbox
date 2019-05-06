import subprocess
from dotenv import load_dotenv

load_dotenv(verbose=True)


cmd = ["python", "-c", "import os; print(os.environ.get('message', '<none>'))"]
subprocess.run(cmd, check=True)
