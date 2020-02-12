import importlib.resources as resources
import botocore

# hmm
contents = list(resources.contents("botocore.data.sqs"))
print(contents)
for name in contents:
    print(name)
