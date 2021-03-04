import sys
import json


# https://registry.terraform.io/providers/hashicorp/external/latest/docs/data-sources/data_source
import random
data = json.load(sys.stdin)
print(json.dumps({"message": f"hello {data['id']}", "n": str(random.random())}))

# fail
# print(, file=sys.stderr)
# sys.exit(1)
