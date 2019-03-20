import time
from yaspin import yaspin

# Context manager:
with yaspin():
    time.sleep(3)  # time consuming code

# Function decorator:
@yaspin(text="Loading...")
def some_operations():
    time.sleep(3)  # time consuming code


some_operations()
