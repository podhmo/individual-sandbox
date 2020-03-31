import dotenv
import os
print(dotenv.load_dotenv(verbose=True))
print(os.environ.get("TOKEN"))
