import time
import random
import json


for i in range(10):
    print(json.dumps({"i": i, "n": random.random()}))
    time.sleep(0.1)
