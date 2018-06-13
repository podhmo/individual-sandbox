import os
for k, v in sorted(os.environ.items()):
    print(k, "@", v)
