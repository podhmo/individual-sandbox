import sys
from ioknife.too import too

cmds = [
    (
        "normal",
        [
            sys.executable,
            "-u",
            "-c",
            "import time; [(print(i) or time.sleep(0.3)) for i in range(10)]",
        ],
    ),
    (
        "with stderr",
        [
            sys.executable,
            "-u",
            "-c",
            "import sys; import time; [(print(i, file=sys.stderr) or time.sleep(0.3)) for i in range(10)]",
        ],
    ),
    (
        "quick",
        [
            sys.executable,
            "-u",
            "-c",
            "import time; [(print(i) or time.sleep(0.2)) for i in range(10)]",
        ],
    ),
    (
        "quick",
        [
            sys.executable,
            "-u",
            "-c",
            "import time; [(print(i) or time.sleep(0.2)) for i in range(10)]",
        ],
    ),
    (
        "quick",
        [
            sys.executable,
            "-u",
            "-c",
            "import time; [(print(i) or time.sleep(0.2)) for i in range(10)]",
        ],
    ),
    (
        "quick",
        [
            sys.executable,
            "-u",
            "-c",
            "import time; [(print(i) or time.sleep(0.2)) for i in range(10)]",
        ],
    ),
]
too(cmds)
