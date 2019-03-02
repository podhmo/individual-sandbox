from dictknife import loading
import jsonschema
import argparse


parser = argparse.ArgumentParser()
parser.add_argument("--schema", required=True)
parser.add_argument("--data", required=True)

args = parser.parse_args()

schema = loading.loadfile(args.schema)
jsonschema.Draft4Validator.check_schema(schema)

data = loading.loadfile(args.data)
for err in jsonschema.Draft4Validator(schema).iter_errors(data):
    print(err)

