import time
from yaspin import yaspin

with yaspin(text="Colors!") as sp:
    # Support all basic termcolor text colors
    colors = ("red", "green", "yellow", "blue", "magenta", "cyan", "white")

    for color in colors:
        sp.color, sp.text = color, color
        time.sleep(1)
