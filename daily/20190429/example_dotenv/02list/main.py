from dotenv import load_dotenv, dotenv_values

load_dotenv(verbose=True)
print(dotenv_values(verbose=True))
print("hmm...")


import os

print({k: os.environ.get(k) for k in dotenv_values()})
