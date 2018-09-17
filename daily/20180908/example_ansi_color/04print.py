import sys

if sys.stdout.isatty():
    COLOR_ON = "[01;34m"
    COLOR_OFF = "[0m"
else:
    COLOR_ON = ""
    COLOR_OFF = ""

print(f"{COLOR_ON}hello{COLOR_OFF} world")
