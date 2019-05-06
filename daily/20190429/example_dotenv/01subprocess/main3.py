import os
import subprocess
from dotenv import load_dotenv

load_dotenv(verbose=True)


cmd = ["python", "-c", "import os; print(os.environ.get('message', '<none>'))"]

myenv = os.environ.copy()
myenv["message"] = "bye, bye"

subprocess.run(cmd, check=True)
subprocess.run(cmd, check=True, env=myenv)
subprocess.run(cmd, check=True)
