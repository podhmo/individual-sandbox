import sys
import json

buf = []
for line in sys.stdin:
    buf.append(line)
    try:
        d = json.loads("\n".join(buf))
    except json.JSONDecodeError as e:
        pass
    else:
        print(d["n"])
        buf.clear()
        sys.stdout.flush()
